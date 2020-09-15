package pipedsl

import pipedsl.common.Errors
import pipedsl.common.Syntax._

import scala.collection.immutable
import scala.collection.immutable.HashMap

class Interpreter(val maxIterations: Int) {

    type Environment = scala.collection.immutable.Map[Id, Any]
    type MemoryEnvironment = scala.collection.immutable.Map[Id, Array[Int]]
    type Functions = scala.collection.immutable.Map[Id, FuncDef]
    type Modules = scala.collection.immutable.Map[Id, ModuleDef]
    
    var functions: Functions = new immutable.HashMap[Id, FuncDef]()
    var modules: Modules = new immutable.HashMap[Id, ModuleDef]()
    var memoryEnv: MemoryEnvironment = new immutable.HashMap[Id, Array[Int]]()
    var iterations: Int = 1

    def interp_expr(e: Expr, env:Environment): Any = e match {
        case i: EInt => i.v
        case b: EBool => b.v
        case o: EBinop => o.op.operate(interp_expr(o.e1, env), interp_expr(o.e2, env)) match {
            case Some(v) => v
            case None => throw Errors.UnexpectedExpr(e)
        }
        case u: EUop => u.op.operate(interp_expr(u.ex, env)) match {
            case Some(v) => v
            case None => throw Errors.UnexpectedExpr(e)
        }
        case v: EVar => env(v.id)
        case r: ERecLiteral => r.fields
        case rf: ERecAccess => {
            val rec = interp_expr(rf.rec, env)
            rec.asInstanceOf[Map[Id, Any]](rf.fieldName)
        }
        case m: EMemAccess => {
            val mem = memoryEnv(m.mem)
            val idx = interp_expr(m.index, env)
            mem(idx.asInstanceOf[Int])
        }
        case t: ETernary => {
            val cond = interp_expr(t.cond, env)
            if (cond.asInstanceOf[Boolean]) {
                interp_expr(t.tval, env)
            } else {
                interp_expr(t.fval, env)
            }
        }
        case be: EBitExtract => {
            val n = interp_expr(be.num, env).asInstanceOf[Int]
            val start = be.start
            val end = be.end
            if (start > end) throw Errors.InvalidBitExtraction(start, end)
            val mask = (~0) << (31 - end) >>> (31 - (end-start)) << start
            (mask & n) >>> start
        }
        case f: EApp => {
            val func: FuncDef = functions.get(f.func).get
            var newEnv = new immutable.HashMap[Id, Any]()
            for (index <- 0 until f.args.length) {
                //add arguments to new environment that will be used for the new function execution 
                newEnv = newEnv + (func.args(index).name -> interp_expr(f.args(index), env))
            }
            interp_function(func, newEnv)
        }
        case ex => throw Errors.UnexpectedExpr(ex)
    }
    

    def interp_command(c: Command, env: Environment): Environment = c match {
        case CSeq(c1, c2) => {
            val e2 = interp_command(c1, env)
            interp_command(c2, e2)
        }
        case CTBar(c1, c2) => {
            val e2 = interp_command(c1, env)
            //TODO also produce and update next cycle values
            interp_command(c2, e2)
        }
        case CIf(cond, tbr, fbr) => {
            val b = interp_expr(cond, env).asInstanceOf[Boolean]
            if (b) {
                interp_command(tbr, env)
            } else {
                interp_command(fbr, env)
            }
        }
        case CAssign(lhs, rhs) => {
            val rval = interp_expr(rhs, env)
            lhs match {
                case EVar(id) => {
                    env.get(id) match {
                        case Some(v) => throw Errors.AlreadySetException(id)
                        case None =>  env + (id -> rval)
                    }
                }
                case _ => throw Errors.UnexpectedExpr(lhs)
            }
        }
        case CRecv(lhs, rhs) => {
            //TODO values available next cycle
            val rval = interp_expr(rhs, env)
            lhs match {
                case EVar(id) => {
                    env.get(id) match {
                        case Some(v) => throw Errors.AlreadySetException(id)
                        case None => {
                            env + (id -> rval)
                        }
                    }
                }
                case EMemAccess(mem, index) => {
                    val memArray = memoryEnv(mem)
                    val idx = interp_expr(index, env)
                    memArray(idx.asInstanceOf[Int]) = rval.asInstanceOf[Int]
                    memoryEnv = memoryEnv + (mem -> memArray)
                    env
                }
            }
        }
        case CExpr(exp) => {
            interp_expr(exp, env)
            env
        }
        case COutput(exp) => {
            val v = interp_expr(exp, env)
            env
        }
        case CReturn(exp) => {
            val r = interp_expr(exp, env)
            env + (Id("__RETURN__") -> r)
        }
        case CCall(id, args) => {
            var newEnv = new immutable.HashMap[Id, Any]()
            val moddef = modules(id)
            for (index <- 0 until args.length) {
                //add arguments to new environment that will be used for the new module execution 
                newEnv = newEnv + (moddef.inputs(index).name -> interp_expr(args(index), env))
            }
            if (iterations < maxIterations) {
                iterations = iterations + 1
                interp_module(moddef, newEnv)
            }
            env
        }
        case _ => env
    }
    
    def interp_function(f: FuncDef, env: Environment): Any = {
        interp_command(f.body, env)(Id("__RETURN__"))
    }
    
    def interp_module(m: ModuleDef, env: Environment): Environment = { 
        interp_command(m.body, env)
    }
    
    def interp_prog(p: Prog, mems: HashMap[String, Array[Int]]): Environment = {
        p.fdefs.foreach(fdef => functions = functions + (fdef.name -> fdef))
        p.moddefs.foreach(moddef => modules = modules + (moddef.name -> moddef))
        mems.keySet.foreach(mem => memoryEnv = memoryEnv + (Id(mem) -> mems.get(mem).get))
        interp_module(p.moddefs(0), new immutable.HashMap[Id, Any]() + (Id("pc") -> 0))
        memoryEnv.keySet.foreach(mem => {println(mem.v); println(memoryEnv.get(mem).get.mkString(", "))})
        memoryEnv
    }
}
