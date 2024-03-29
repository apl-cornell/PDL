package pipedsl.typechecker

import com.microsoft.z3.{AST => Z3AST, BoolExpr => Z3BoolExpr, Context => Z3Context}
import pipedsl.typechecker.TypeInferenceWrapper.apply_subst_typ
import pipedsl.common.Errors._
import pipedsl.common.Locks._
import pipedsl.common.Syntax._
import pipedsl.common.Utilities._

object Environments {

    val EmptyTypeEnv: Environment[Id, Type] = TypeEnv()
    val EmptyLockEnv: Environment[Id, LockState] = LockEnv()
    val EmptyBoolEnv: Environment[Id, Boolean] = BoolEnv()
    val EmptyIntEnv : Environment[Id, (Int, Int)] = IntEnv()

    sealed trait Environment[U, T] {
        /**
         * Use application syntax to get a type bindings for an id.
         */
        def apply(key: U): T
        def add(name: U, typ: T): Environment[U, T]
        def get(name: U): Option[T]
        def getMappedKeys(): Set[U]

        /**
         * Create a new Environment with all the bindings in binds added to the
         * current scope.
         *
         * @param binds A scope with bindings to be added to the environment.
         * @return A new environment with all the bindings in the environment.
         */
        def ++(binds: Map[U, T]): Environment[U, T] =
            binds.foldLeft[Environment[U, T]](this)({ case (e, b) => e.add(b._1, b._2) })

        def --(binds: Set[U]): Environment[U, T] =
            binds.foldLeft[Environment[U, T]](this)({ case (e, b) => e.remove(b) })

        def remove(key: U): Environment[U, T]

        def intersect(other: Environment[U, T]): Environment[U, T]
        def union(other: Environment[U, T]): Environment[U, T]
      
        def filter(value: T): Environment[U, T] = {
            this.getMappedKeys().filter(key => this(key) != value).foldLeft(this)((env, key) => {
                env.remove(key)
            })
        }

        /**
         * Fill a new environment by mapping a function over the values
         * of this environment
         * @param mapfunc The map function to apply to all of the values of the environment
         * @param emptyEnv The new environment to add (via the @add method)
         *                 all of the transformed values to.
         * @return The given environment (emptyEnv) but with the transformed values added
         */
        def map(mapfunc: T => T, emptyEnv: Environment[U, T]): Environment[U, T] = {
            this.getMappedKeys().foldLeft(emptyEnv)((e, k) => {
                e.add(k, mapfunc(this(k)))
            })
        }
    }

    case class TypeEnv(
            typeMap: Map[Id, Type] = Map()) extends Environment[Id, Type] {
        
        override def apply(id: Id) = this.get(id).getOrThrow(MissingType(id.pos, id.v))
        
        override def add(name: Id, typ: Type): Environment[Id, Type] = typeMap.get(name) match {
            case Some(t) => throw AlreadyBoundType(name.pos, name.v, t, typ)
            case None => this.copy(typeMap = typeMap + (name -> typ))
        }
        override def remove(name: Id): Environment[Id, Type] = {
            TypeEnv(this.typeMap - name)
        }
        override def get(name: Id): Option[Type] = typeMap.get(name)
        override def getMappedKeys(): Set[Id] = typeMap.keySet
        override def intersect(other: Environment[Id, Type]): Environment[Id, Type] = {
            TypeEnv(
                getMappedKeys().intersect(other.getMappedKeys())
                        .filter( id => {this(id) == other(id) })
                        .map(k => { k -> this(k) } )
                        .toMap
            )
        }

        override def union(other: Environment[Id, Type]): Environment[Id, Type] = {
            TypeEnv(other.getMappedKeys().foldLeft(typeMap)((m, id) => {
                val otherval = other(id)
                if (m.contains(id)) {
                    if (otherval != m(id)) throw IllegalTypeMerge(id.pos, id, m(id), otherval)
                    m
                } else {
                    m + (id -> otherval)
                }
            }))
        }
        def apply_subst_typeenv(subst: List[(Id, Type)]): TypeEnv = TypeEnv(typeMap.foldLeft[Map[Id, Type]](Map())((e, kv) => e + (kv._1 -> apply_subst_typ(subst, kv._2))))
    }

    /*this is used for the port checker so that we can properly merge number*/
    /*of ports on different if branches*/
    case class IntEnv(portMap : Map[Id, (Int, Int)] = Map())
      extends Environment[Id, (Int, Int)]
        {
            override def apply(key: Id): (Int, Int) = this.get(key)
              .getOrElse((0, 0))

            /*actually ignores the number you pass it so that the counting is*/
            /*maintained*/
            override def add(name: Id, typ: (Int, Int))
            : Environment[Id, (Int, Int)] = portMap.get(name) match
            {
                case Some((read, write)) =>
                    typ match
                    {
                        case (1, 0) =>
                            IntEnv(portMap = portMap - name + (name -> (read+1, write)))
                        case (0, 1) =>
                            IntEnv(portMap = portMap - name + (name -> (read, write+1)))
                        case _ =>
                            throw new RuntimeException("Only add (1, 0) or (0, 1)")
                    }
                case None =>
                    typ match
                    {
                        case (1, 0) | (0, 1) =>
                            IntEnv(portMap = portMap + (name -> typ))
                        case _ =>
                            throw new RuntimeException("Only add (1, 0) or (0, 1)")
                    }
            }
            override def get(name: Id): Option[(Int, Int)] = portMap.get(name)

        override def getMappedKeys(): Set[Id] = portMap.keySet
        override def remove(key: Id): Environment[Id, (Int, Int)] =
            IntEnv(portMap = portMap - key)

        override def intersect(other: Environment[Id, (Int, Int)])
        : Environment[Id, (Int, Int)] = IntEnv(
            getMappedKeys().intersect(other.getMappedKeys())
              .filter( id => {this(id) == other(id) })
              .map(k => { k -> this(k) } )
              .toMap
        )

        override def union(other: Environment[Id, (Int, Int)])
        : Environment[Id, (Int, Int)]
        =
            {
                def pair_max(a:(Int, Int), b:(Int, Int)) =
                    (Math.max(a._1, b._1), Math.max(a._2, b._2))
                IntEnv(getMappedKeys().union(other.getMappedKeys()).foldLeft
                (Map(): Map[Id, (Int, Int)])
                ((acc, id) => acc + (id -> ((this.get(id), other.get(id)) match

                {
                    case (Some(x), Some(y)) => pair_max(x, y)
                    case (Some(x), None) => x
                    case (None, Some(y)) => y
                    case (None, None) => throw new RuntimeException("never happens")
                }))))
            }
    }

    case class LockEnv(lockMap: Map[Id, LockState] = Map()) extends Environment[Id, LockState] {
        override def apply(id: Id) = this.get(id).getOrThrow(MissingType(id.pos, id.v))

        private def updateMapping(n: Id, ns: LockState): Environment[Id, LockState] = {
            this.copy(lockMap = lockMap + (n -> ns))
        }
        //only allow legal lock state transitions
        override def add(name: Id, ns: LockState): Environment[Id, LockState] = if (lockMap.contains(name)) {
            (lockMap(name), ns) match {
                case (Free, Acquired | Reserved) => updateMapping(name, ns)
                case (Reserved, Acquired) => updateMapping(name, ns)
                case (Acquired, Released) => updateMapping(name, ns)
                case (_, _) => throw IllegalLockModification(name.pos, name.v, lockMap(name), ns)
            }
        } else {
            updateMapping(name, ns)
        }

        override def remove(name: Id): Environment[Id, LockState] = {
            LockEnv(this.lockMap - name)
        }
        override def get(name: Id): Option[LockState] = lockMap.get(name)
        override def getMappedKeys(): Set[Id] = lockMap.keySet
        //ensure that all "Acquired" or "Reserved" locks are in same state
        //promote all unmapped or "Free" locks to "Released" if they are released in other env
        override def intersect(other: Environment[Id, LockState]): Environment[Id, LockState] = {
            val allIds = this.getMappedKeys().union(other.getMappedKeys())
            LockEnv(allIds.foldLeft[Map[Id, LockState]](Map())
              ( (env, id) => (lockMap(id), other(id)) match {
                  case (l, r) if l == r => env + (id -> l)
                  case (Released, Free) => env + (id -> Released)
                  case (Free, Released) => env + (id -> Released)
                  case (l@_, r@_) => throw IllegalLockMerge(id.pos, id.v, l, r)
            }))
        }
        //Combine changes to Free locks, but otherwise
        //require all entries to match exactly
        override def union(other: Environment[Id, LockState]): Environment[Id, LockState] = {
            LockEnv(other.getMappedKeys().foldLeft(lockMap)((m, id) => {
                val otherval = other(id)
                if (m.contains(id)) {
                    if (otherval != m(id) && otherval != Free) throw IllegalLockMerge(id.pos, id.v, m(id), otherval)
                    m
                } else {
                    m + (id -> otherval)
                }
            }))
        }
    }
    case class BoolEnv(boolSet: Set[Id] = Set()) extends Environment[Id, Boolean] {
        override def apply(id: Id) = this.get(id).getOrThrow(MissingType(id.pos, id.v))
        override def add(name: Id, b: Boolean): Environment[Id, Boolean] =
            if (b) BoolEnv(boolSet + name) else BoolEnv(boolSet - name)
        override def remove(name: Id): Environment[Id, Boolean] =
            BoolEnv(boolSet - name)
        override def get(name: Id): Option[Boolean] = Some(boolSet(name))
        override def getMappedKeys(): Set[Id] = boolSet
        override def intersect(other: Environment[Id, Boolean]): Environment[Id, Boolean] =
            BoolEnv(boolSet.intersect(other.getMappedKeys()))
        override def union(other: Environment[Id, Boolean]): Environment[Id, Boolean] =
            BoolEnv(boolSet.union(other.getMappedKeys()))
    }


    case class ConditionalEnv(m: Map[Id, Z3AST] = Map(), ctx: Z3Context)
     extends Environment[Id, Z3AST] {
        override def apply(key: Id) = this.get(key).getOrThrow(MissingType
        (key.pos, key.v))
        private def updateMapping(n: Id, ns: Z3AST): Environment[Id, Z3AST] = {
            this.copy(m = m + (n -> ns))
        }
        //only allow legal lock state transitions
        override def add(name: Id, ns: Z3AST): Environment[Id, Z3AST] =
            updateMapping(name, ns)

        override def remove(key: Id): Environment[Id, Z3AST] = ConditionalEnv(m - key, ctx)

        override def get(name: Id): Option[Z3AST] = m.get(name)

        override def getMappedKeys(): Set[Id] = m.keySet

        override def intersect(other: Environment[Id, Z3AST]): Environment[Id, Z3AST] = {
            var newMap: Map[Id, Z3AST] = Map()
            for (key <- (this.getMappedKeys() ++ other.getMappedKeys())) {
                (this.get(key), other.get(key)) match {
                    case (Some(v1), Some(v2)) => {
                        //Just takes the and of both states to merge. This works because if state
                        // is unchanged from a branch, it will be "cond implies a and (not cond) implies a"
                        val and = ctx.mkAnd(v1.asInstanceOf[Z3BoolExpr] , v2.asInstanceOf[Z3BoolExpr])
                        newMap = newMap + (key -> and)
                    }
                    case (Some(v1), None) => newMap = newMap + (key -> v1)
                    case (None, Some(v2)) => newMap = newMap + (key -> v2)
                    case _ => ()
                }
            }
            this.copy(m = newMap)
        }
        //This is filler code, I don't think we ever actually need this
        override def union(other: Environment[Id, Z3AST]): Environment[Id, Z3AST] = other
    }

    //TODO merge code for this into the plain ConditionalEnv to reduce copy-pasta
    case class ConditionalLockEnv(lockMap: Map[LockArg, Z3AST] = Map(), ctx: Z3Context)
      extends Environment[LockArg, Z3AST] {
        override def apply(key: LockArg) = this.get(key).getOrThrow(MissingType(key.pos, key.id.v))
        private def updateMapping(n: LockArg, ns: Z3AST): Environment[LockArg, Z3AST] = {
            this.copy(lockMap = lockMap + (n -> ns))
        }
        //only allow legal lock state transitions
        override def add(name: LockArg, ns: Z3AST): Environment[LockArg, Z3AST] = 
            updateMapping(name, ns)

        override def remove(key: LockArg): Environment[LockArg, Z3AST] = ConditionalLockEnv(lockMap - key, ctx)

        override def get(name: LockArg): Option[Z3AST] = lockMap.get(name)

        override def getMappedKeys(): Set[LockArg] = lockMap.keySet

        override def intersect(other: Environment[LockArg, Z3AST]): Environment[LockArg, Z3AST] = {
            var newMap: Map[LockArg, Z3AST] = Map()
            for (key <- other.getMappedKeys()) {
                this.get(key) match {
                    case Some(value) => {
                        //Just takes the and of both lock states to merge. This works because if lock state 
                        // is unchanged from a branch, it will be "cond implies a and (not cond) implies a"
                        val and = ctx.mkAnd(value.asInstanceOf[Z3BoolExpr] , other(key).asInstanceOf[Z3BoolExpr])
                        newMap = newMap + (key -> and)
                    }
                    case None => newMap = newMap + (key -> other(key))
                }
            }
            this.copy(lockMap = newMap)
        }

        //This is filler code, I don't think we ever actually need this
        override def union(other: Environment[LockArg, Z3AST]): Environment[LockArg, Z3AST] = other
        
    }
}
