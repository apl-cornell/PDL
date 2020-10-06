package pipedsl.typechecker

import pipedsl.common.Errors.{AlreadyBoundType, IllegalLockMerge, IllegalLockModification, IllegalTypeMerge, MissingType}
import pipedsl.common.Locks._
import pipedsl.common.Syntax._
import pipedsl.common.Utilities._

object Environments {

    val EmptyTypeEnv: Environment[Type] = TypeEnv()
    val EmptyLockEnv: Environment[LockState] = LockEnv()
    val EmptyBoolEnv: Environment[Boolean] = BoolEnv()

    sealed trait Environment[T] {
        /**
         * Use application syntax to get a type bindings for an id.
         */
        def apply(id: Id): T = this.get(id).getOrThrow(MissingType(id.pos, id.v))
        def add(name: Id, typ: T): Environment[T]
        def get(name: Id): Option[T]
        def getMappedIds: Set[Id]

        /**
         * Create a new Environment with all the bindings in binds added to the
         * current scope.
         *
         * @param binds A scope with bindings to be added to the environment.
         * @return A new environment with all the bindings in the environment.
         */
        def ++(binds: Map[Id, T]): Environment[T] =
            binds.foldLeft[Environment[T]](this)({ case (e, b) => e.add(b._1, b._2) })

        def --(binds: Set[Id]): Environment[T] =
            binds.foldLeft[Environment[T]](this)({ case (e, b) => e.remove(b) })

        def remove(id: Id): Environment[T]

        def intersect(other: Environment[T]): Environment[T]
        def union(other: Environment[T]): Environment[T]

        def filter(value: T): Environment[T] = {
            this.getMappedIds.filter(id => this(id) != value).foldLeft(this)((env, id) => {
                env.remove(id)
            })
        }
    }

    case class TypeEnv(
            typeMap: Map[Id, Type] = Map()) extends Environment[Type] {

        override def add(name: Id, typ: Type): Environment[Type] = typeMap.get(name) match {
            case Some(t) => throw AlreadyBoundType(name.pos, name.v, t, typ)
            case None => this.copy(typeMap = typeMap + (name -> typ))
        }
        override def remove(name: Id): Environment[Type] = {
            TypeEnv(this.typeMap - name)
        }
        override def get(name: Id): Option[Type] = typeMap.get(name) match {
            case Some(TNamedType(n)) => this.get(n)
            case other@_ => other
        }
        override def getMappedIds: Set[Id] = typeMap.keySet
        override def intersect(other: Environment[Type]): Environment[Type] = {
            TypeEnv(
                getMappedIds.intersect(other.getMappedIds)
                        .filter( id => {this(id) == other(id) })
                        .map(k => { k -> this(k) } )
                        .toMap
            )
        }
        override def union(other: Environment[Type]): Environment[Type] = {
            TypeEnv(other.getMappedIds.foldLeft(typeMap)((m, id) => {
                val otherval = other(id)
                if (m.contains(id)) {
                    if (otherval != m(id)) throw IllegalTypeMerge(id.pos, id, m(id), otherval)
                    m
                } else {
                    m + (id -> otherval)
                }
            }))
        }
    }

    case class LockEnv(lockMap: Map[Id, LockState] = Map()) extends Environment[LockState] {

        private def updateMapping(n: Id, ns: LockState): Environment[LockState] = {
            this.copy(lockMap = lockMap + (n -> ns))
        }
        //only allow legal lock state transitions
        override def add(name: Id, ns: LockState): Environment[LockState] = if (lockMap.contains(name)) {
            (lockMap(name), ns) match {
                case (Free, Acquired | Reserved) => updateMapping(name, ns)
                case (Reserved, Acquired) => updateMapping(name, ns)
                case (Acquired, Released) => updateMapping(name, ns)
                case (_, _) => throw IllegalLockModification(name.pos, name.v, lockMap(name), ns)
            }
        } else {
            updateMapping(name, ns)
        }

        override def remove(name: Id): Environment[LockState] = {
            LockEnv(this.lockMap - name)
        }
        override def get(name: Id): Option[LockState] = lockMap.get(name)
        override def getMappedIds: Set[Id] = lockMap.keySet
        //ensure that all "Acquired" or "Reserved" locks are in same state
        //promote all unmapped or "Free" locks to "Released" if they are released in other env
        override def intersect(other: Environment[LockState]): Environment[LockState] = {
            val allIds = this.getMappedIds.union(other.getMappedIds)
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
        override def union(other: Environment[LockState]): Environment[LockState] = {
            LockEnv(other.getMappedIds.foldLeft(lockMap)((m, id) => {
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
    case class BoolEnv(boolSet: Set[Id] = Set()) extends Environment[Boolean] {
        override def add(name: Id, b: Boolean): Environment[Boolean] =
            if (b) BoolEnv(boolSet + name) else BoolEnv(boolSet - name)
        override def remove(name: Id): Environment[Boolean] =
            BoolEnv(boolSet - name)
        override def get(name: Id): Option[Boolean] = Some(boolSet(name))
        override def getMappedIds: Set[Id] = boolSet
        override def intersect(other: Environment[Boolean]): Environment[Boolean] =
            BoolEnv(boolSet.intersect(other.getMappedIds))
        override def union(other: Environment[Boolean]): Environment[Boolean] =
            BoolEnv(boolSet.union(other.getMappedIds))
    }
}
