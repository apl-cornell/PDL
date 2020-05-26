package pipedsl

import common.Syntax._
import pipedsl.common.Errors

import scala.collection.immutable

class Interpreter {

    def interp_expr(e: Expr): Long = e match {
        case i: EInt => i.v
        case o: EBinop => o.op.toFun match {
            case Some(f) => f(interp_expr(o.e1), interp_expr(o.e2))
            case None => throw Errors.UnexpectedExpr(o)
        }
        case ex => throw Errors.UnexpectedExpr(ex)
    }

    type Environment = scala.collection.immutable.Map[Id, Number]

    def interp_command(c: Command): Unit = {
        interp_command_helper(c, new immutable.HashMap[Id, Number]())
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
        case CAssign(lhs, rhs) => {
            val rval = interp_expr(rhs)
            lhs match {
                case EVar(id) => {
                    env + (id -> rval)
                }
                case _ => throw Errors.UnexpectedExpr(lhs)
            }
        }
        case _ => env
    }
}
