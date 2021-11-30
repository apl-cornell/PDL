package pipedsl.typechecker

import pipedsl.common.{Locks, Syntax, Errors}
import pipedsl.common.Syntax._
import pipedsl.common.Errors._
import pipedsl.typechecker.TypeChecker.TypeChecks
import pipedsl.typechecker.Environments._

import scala.collection.mutable

/**
 * This annotates ports onto reads, writes, reserves, and releases.
 * It also checks in a fairly rudimentary way that the same pipe isn't called
 * multiple times in the same cycle (though this can potentially be updated
 * for superscalar execution). This bit is NOT done with the SMT solver, so
 * other than knowing abut if-else it isn't very smart. Ports are simply
 * assigned in order as operations are encountered in a cycle, with if-else
 * being done "in parallel" and then having a max taken at the end. For
 * example,
 * if(cond)
 * { x = rf[rs1]; y = rf[rs2] }
 * else
 * { z = rf[rs3]; }
 * w = rf[rs4]
 * will assign x to read port 1, y to read port 2, and z to read port 1. z
 * will then be assigned to read port 3
 */
class PortChecker(port_warn :Boolean) extends TypeChecks[Id, (Int, Int)]
{

  private val optimalPorts = mutable.HashMap.empty[Id, (Int, Int)]
  private val modLims = mutable.HashMap.empty[Id, (Int, Int)]
  private val reserveMap = mutable.HashMap.empty[LockArg, Int]

  override def
  emptyEnv(): Environment[Id, (Int, Int)] =
    EmptyIntEnv

  override def
  checkExt(e: ExternDef, env:Environment[Id, (Int, Int)]): Environment[Id, (Int, Int)] = env

  override def
  /*functions are combinational and cannot do anything with mem/locks*/
  checkFunc(f: FuncDef, env: Environment[Id, (Int, Int)])
  : Environment[Id, (Int, Int)] = env

  override def
  checkCircuit(c: Circuit, env: Environment[Id, (Int, Int)])
  : Environment[Id, (Int, Int)] = env

  override def
  checkModule(m: ModuleDef, env: Environment[Id, (Int, Int)])
  : Environment[Id, (Int, Int)] =
    {
      modLims.clear()
      optimalPorts.clear()
      reserveMap.clear()
      m.modules.foreach(mod => mod.typ match {
        case TMemType(_, _, _, _, r, w) => modLims.addOne((mod.name, (r, w)))
        case TLockedMemType(TMemType(_, _, _, _, r, w), _, _) =>
          modLims.addOne((mod.name, (r, w)))
        case _ : TModType => modLims.addOne((mod.name, (1, 1)))
        case _ =>
      })
      val port_map = checkPipe(m.body, emptyEnv())
      if(port_warn)
        port_map.getMappedKeys().foreach(mem =>
          {
            /*sadly we are reusing the int pair to mean sth dif on mem/locks*/
            println(mem.v + " r/res: " + port_map(mem)._1)
            println(mem.v + " w/rel: " + port_map(mem)._2)
          })

      m.modules.foreach(mod =>
        {
          val (r, w) = port_map.get(mod.name).getOrElse((0, 0))
          val (ro, wo) = optimalPorts.getOrElse(mod.name, (0, 0))
          val ass_ports = (rp :Int, wp :Int) =>
            {
              if(r > rp)
                throw InsufficientPorts(mod.pos, "read", mod.name, rp, r)
              else if (ro > rp && port_warn)
                throw SuboptimalPorts(mod.pos, "read", mod.name, rp, r)
              if(w > wp)
                throw InsufficientPorts(mod.pos, "write", mod.name, wp, w)
              else if (wo > wp && port_warn)
                throw SuboptimalPorts(mod.pos, "write", mod.name, wp, w)
            }
          mod.typ match
          {
            case TMemType(_, _, _, _, rp, wp) =>
              ass_ports(rp, wp)
            case TLockedMemType(TMemType(_, _, _, _, rp, wp), _, _) =>
              ass_ports(rp, wp)
            case _ =>
          }
        }
      )
      port_map
    }

  def
  /*this function lets us use recursion to split across the time barriers*/
  checkPipe(c: Command,
            strt_env :Environment[Id, (Int, Int)])
  :Environment[Id, (Int, Int)] =
    {
      val start_env = strt_env//callees.foldLeft(strt_env)((nv, id) => nv.remove(id))
       c match
      {
        case CTBar(c1@CTBar(_, _), c2@CTBar(_, _)) => checkPipe(c2, checkPipe(c1, start_env))

        case CTBar(c1@CTBar(_, _), c2) =>
          val env1 = checkPipe(c1, start_env)
          checkCommand(c2, emptyEnv(), env1)
        case CTBar(c1, c2@CTBar(_, _)) =>
          checkPipe(c2, checkCommand(c1, emptyEnv(), start_env))
        case CTBar(c1, c2) =>
          val env1 = checkCommand(c1, emptyEnv(), start_env)
          checkCommand(c2, emptyEnv(), env1)
        case _ => checkCommand(c, emptyEnv(), start_env)
      }
    }


  /**
   * checks a command within a time barrier
   * @param c the command to check
   * @param env the environment to check in
   * @return a new environment including the results of checking c
   */
  def
  checkCommand(c: Command,
               env: Environment[Id, (Int, Int)],
               start_env: Environment[Id, (Int, Int)])
  : Environment[Id, (Int, Int)] = c match
  {
    case CSeq(c1, c2) =>
      checkCommand(c2, checkCommand(c1, env, start_env), start_env)
    case CTBar(_, _) => checkPipe(c, start_env)
    case CIf(_, cons, alt) =>
      checkCommand(cons, env, start_env).union(checkCommand(alt, env, start_env))
    case CAssign(_, data) =>
      checkExpr(data, env, start_env)
    case CRecv(EVar(_), EMemAccess(mem, EVar(_), _, _, _, _)) =>
      /*asynch read*/
      {mem.typ.get match {
        case TMemType(_, _, rlat, _, _, _) =>
          rlat
        case TLockedMemType(TMemType(_, _, rlat, _, _, _), _, _) =>
          rlat
        case _ => throw new RuntimeException("no")
      }} match {
        case Latency.Asynchronous =>
         /* println("asynch read")*/
          val ret = env.add(mem, (0, 1))
          var port = (ret(mem)._2 + start_env(mem)._2) % modLims(mem)._2
          if (port == 0) port = modLims(mem)._2
          /*println("port: " + port)*/
          c.portNum = Some(port)
          //      println(s"write port: $port")
          //      println(s"limit: ${modLims(mem)._2}")
          if (ret(mem)._2 > modLims(mem)._2)
            throw InsufficientPorts(mem.pos, "read/write", mem, modLims(mem)._2, ret(mem)._2)
          val cur_opt = optimalPorts.getOrElse(mem, (0, 0))
          optimalPorts.update(mem, (cur_opt._1, cur_opt._2 + 1))
          ret
        case _ =>
        /*  println("synch read")*/
          val ret = env.add(mem, (1, 0))
          var port = (ret(mem)._1 + start_env(mem)._1) % modLims(mem)._1
          if (port == 0) port = modLims(mem)._1
          c.portNum = Some(port)
          if (ret(mem)._1 > modLims(mem)._1)
            throw InsufficientPorts(mem.pos, "read", mem, modLims(mem)._1, ret(mem)._1)
          val cur_opt = optimalPorts.getOrElse(mem, (0, 0))
          optimalPorts.update(mem, (cur_opt._1 + 1, cur_opt._2))
          ret
      }

    case CRecv(EMemAccess(mem, _, _, _, _, _), _) =>
      /*any write, asynch or sequential*/
      /*println("any write: " + mem + " : " + env(mem) + " : " + start_env(mem))*/
      val ret = env.add(mem, (0, 1))
      var port = (ret(mem)._2 + start_env(mem)._2) % modLims(mem)._2
      if (port == 0) port = modLims(mem)._2
      /*println("port: " + port)*/
      c.portNum = Some(port)
//      println(s"write port: $port")
//      println(s"limit: ${modLims(mem)._2}")
      if (ret(mem)._2 > modLims(mem)._2)
        throw InsufficientPorts(mem.pos, "write", mem, modLims(mem)._2, ret(mem)._2)
      val cur_opt = optimalPorts.getOrElse(mem, (0, 0))
      optimalPorts.update(mem, (cur_opt._1, cur_opt._2 + 1))
      /*println(optimalPorts(mem))*/
      ret
    case CRecv(EVar(_), data) =>
      checkExpr(data, env, start_env)
    case CSpecCall(_, pipe, args) =>
      //callees += pipe
      val nenv = args.foldLeft(env)((acc,e) => checkExpr(e, acc, start_env))
      nenv.get(pipe) match
      {
        case Some((1, 0)) =>
          throw NoSuperScalar(pipe)
        case _ =>
          val ret = nenv.add(pipe, (1, 0))
          c.portNum = Some(ret(pipe)._1)
          ret
      }
    case CVerify(_, args,_, _, _) =>
      args.foldLeft(env)((acc, e) => checkExpr(e, acc, start_env))
     case COutput(exp) => checkExpr(exp, env, start_env)
    case CReturn(exp) => checkExpr(exp, env, start_env)
    case CExpr(exp) => checkExpr(exp, env, start_env)
    case CLockOp(mem, op, lockType, _, _) =>
      val mangled = lockType match
      {
        case Some(Syntax.LockRead)  => Id(mem.id.v + "?r")
        case Some(Syntax.LockWrite) => Id(mem.id.v + "?w")
        case None => Id(mem.id.v + "?g")
      }
      op match
      {
        case Locks.Reserved =>
          val ret = env.add(mangled, (1, 0))
          val limit =
            if (lockType.contains(Syntax.LockWrite))
            modLims(mem.id)._2
          else
            modLims(mem.id)._1
          
          var port = (ret(mangled)._1 + start_env(mangled)._1) % limit
          if (port == 0) port = limit
          reserveMap.update (mem, port)
          c.portNum = Some(port)
          if (ret(mangled)._1 > limit)
            throw InsufficientPorts(c.pos, lockType match
            {
              case Some(Syntax.LockRead) => "read"
              case Some(Syntax.LockWrite) => "write"
              case None => "general"
            }, mem.id, limit, ret(mangled)._1)
          ret
        case Locks.Acquired =>
          val port = reserveMap(mem)
          c.portNum = Some(port)
          env
        case Locks.Released =>
          val port = reserveMap(mem)
          c.portNum = Some(port)
          env
        case _ => env
      }
    case CSplit(cases, default) =>
      cases.foldLeft(checkCommand(default, env, start_env))((enviro, comm) =>
        checkCommand(comm.body, env, start_env).union(enviro))
   case _: InternalCommand => env
    case _ => env
  }

  /**
   * recursively annotates ports on a single expression
   * @param e the expression to annotate
   * @param env the environment to check for existing mem/lock ops
   * @return the new environment with the results of e taken into account
   */
  def
  checkExpr(e: Expr,
            env: Environment[Id, (Int, Int)],
            start_env: Environment[Id, (Int, Int)])
  : Environment[Id, (Int, Int)] = e match
  {
    case EIsValid(ex) => checkExpr(ex, env, start_env)
    case EFromMaybe(ex) => checkExpr(ex, env, start_env)
    case EToMaybe(ex) => checkExpr(ex, env, start_env)
    case EUop(_, ex) => checkExpr(ex, env, start_env)
    case EBinop(_, e1, e2) =>
      checkExpr(e2, checkExpr(e1, env, start_env), start_env)
    case ERecAccess(rec, _) => checkExpr(rec, env, start_env)
    case ERecLiteral(fields) =>
      fields.foldLeft(env)((en, p) => checkExpr(p._2, en, start_env))
    case EMemAccess(mem, _, _, _, _, _) =>
      /*combinational read*/
//      println("combinational read")
      val ret = env.add(mem, (1, 0))
      var port = (ret(mem)._1  + start_env(mem)._1) % modLims(mem)._1
      if (port == 0) port = modLims(mem)._1
      e.portNum = Some(port)
      if (ret(mem)._1 > modLims(mem)._1)
        throw InsufficientPorts(e.pos, "read", mem, modLims(mem)._1, ret(mem)._1)
      val cur_opt = optimalPorts.getOrElse(mem, (0, 0))
      optimalPorts.update(mem, (cur_opt._1 + 1, cur_opt._2))
      ret
    case EBitExtract(num, _, _) => checkExpr(num, env, start_env)
    case ETernary(cond, tval, fval) =>
      val cond_env = checkExpr(cond, env, start_env)
      checkExpr(tval, cond_env, start_env)
        .union(checkExpr(fval, cond_env, start_env))
    case EApp(_, args) =>
      args.foldLeft(env)((acc, e) => checkExpr(e, acc, start_env))
    case ECall(mod, _, args) =>
      /*perhaps we should have some restrictions on calling the same module*/
      /*twice in a single cycle. I think I said that should be a nono*/
      //callees += mod
      val nenv = args.foldLeft(env)((acc, e) => checkExpr(e, acc, start_env))
      nenv.get(mod) match
    {
      case Some((1, 0)) =>
        throw NoSuperScalar(mod)
      case _ =>
        /*we can annotate this so that if potentially we support superscalar*/
        /*pipes at some point this would be relevant*/
        val ret = nenv.add(mod, (1, 0))
        e.portNum = Some(ret(mod)._1)
        ret
    }
    case EVar(_) => env
    case ECast(_, exp) => checkExpr(exp, env, start_env)
    case _: CirExpr => env /*not sure about this part?*/
    case _ => env
  }
}
