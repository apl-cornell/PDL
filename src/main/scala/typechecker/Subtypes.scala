package pipedsl.typechecker

import pipedsl.common.Syntax._

object Subtypes {

    def isSubtype(t1: Type, t2: Type): Boolean =  {
        true //TODO, actually check
    }

    def isEquiv(t1: Type, t2: Type): Boolean = {
        isSubtype(t1, t2) && isSubtype(t2, t1)
    }
}
