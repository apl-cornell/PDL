package pipedsl.typechecker

import pipedsl.common.Syntax._

object Subtypes {

    def isSpeculativeSubtype(t1: Type, t2:Type): Boolean = {
        val isSub = isSubtype(t1, t2)
        //if t2 is maybe speculative then t1 can be anything. else both must be nonspeculative
        val okSpec = t2.maybeSpec || !t1.maybeSpec
        isSub && okSpec
    }
    def isSubtype(t1: Type, t2: Type): Boolean =  (t1, t2) match {
        case (TSizedInt(l1, u1), TSizedInt(l2, u2)) => l1 == l2 && u1 == u2
        case (TRecType(_, f1), TRecType(_, f2)) =>
            //f1 must contain all of the named fields of f2 and they can be subtypes of f2's fields
            f2.forall(e => {
                f1.get(e._1) match {
                    case Some(v) if isSubtype(v, e._2) => true
                    case _ => false
                }
            })
        case (TFun(arg1, r1), TFun(arg2, r2)) =>
            if (arg1.length != arg2.length) {
                false
            } else {
                //contravariant arg types
                if(arg1.zip(arg2).forall(args => isSubtype(args._2, args._1))) {
                    //covariant return type
                    isSubtype(r1, r2)
                } else {
                    false
                }
            }
        case _ => areEqual(t1, t2)
    }

    def areEqual(t1: Type, t2: Type): Boolean =  (t1, t2) match {
        case (TSizedInt(l1, u1), TBool()) => l1 == 1 && u1
        case (TBool(), TSizedInt(l1, u1)) => l1 == 1 && u1
        case (TSizedInt(l1, u1), TSizedInt(l2, u2)) => l1 == l2 && u1 == u2
        case (TMemType(e1, as1, r1, w1), TMemType(e2, as2, r2, w2)) => areEqual(e1, e2) &&
          as1 == as2 && r1 == r2 && w1 == w2
        case _ => t1 == t2
    }
}
