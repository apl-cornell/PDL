package pipedsl

import common.Syntax._
import pipedsl.common.Errors

import scala.collection.immutable

class Interpreter {

    type Environment = scala.collection.immutable.Map[Id, Any]

    def interp_expr(e: Expr, env:Environment): Any = e match {
        case i: EInt => i.v
        case o: EBinop => o.op.apply(interp_expr(o.e1, env), interp_expr(o.e2, env)) match {
            case Some(v) => v
            case None => throw Errors.UnexpectedExpr(e)
        }
        case v: EVar => env(v.id)
        case r: ERecLiteral => r.fields
        case rf: ERecAccess => {
            val rec = interp_expr(rf.rec, env)
            rec.asInstanceOf[Map[Id, Any]](rf.fieldName)
        }
        case ex => throw Errors.UnexpectedExpr(ex)
    }



    def interp_command(c: Command): Unit = {
        interp_command_helper(c, new immutable.HashMap[Id, Any]())
    }

    def interp_command_helper(c: Command, env: Environment): Environment = c match {
        case CSeq(c1, c2) => {
            val e2 = interp_command_helper(c1, env)
            interp_command_helper(c2, e2)
        }
        case CTBar(c1, c2) => {
            val e2 = interp_command_helper(c1, env)
            interp_command_helper(c2, e2)
        }
        case CIf(cond, tbr, fbr) => {
            val b = interp_expr(cond, env)
            env
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
        case CExpr(exp) => {
            interp_expr(exp, env)
            env
        }
        case COutput(exp) => {
            val v = interp_expr(exp, env)
            println(v)
            env
        }
        case _ => env
    }
}
