package pipedsl.typechecker

import pipedsl.common.Errors.{AlreadyBoundType, MissingType}
import pipedsl.common.Syntax._
import pipedsl.common.Utilities._

object Environments {

    sealed trait TypeEnvironment {
        /**
         * Use application syntax to get a type bindings for [[id]].
         */
        def apply(id: Id): Type = this.get(id).getOrThrow(MissingType(id.pos, id.v))
        def add(name: Id, typ: Type): TypeEnvironment
        def get(name: Id): Option[Type]
        def getMappedIds(): Set[Id]

        /**
         * Create a new Environment with all the bindings in [[binds]] added to the
         * current scope.
         *
         * @param binds A scope with bindings to be added to the environment.
         * @returns A new environment with all the bindings in the environment.
         */
        def ++(binds: Map[Id, Type]): TypeEnvironment =
            binds.foldLeft[TypeEnvironment](this)({ case (e, b) => e.add(b._1, b._2) })

        def intersect(other: TypeEnvironment): TypeEnvironment
    }

    case class Env(
            typeMap: Map[Id, Type] = Map()) extends TypeEnvironment {

        override def add(name: Id, typ: Type): TypeEnvironment = typeMap.get(name) match {
            case Some(t) => throw AlreadyBoundType(name.pos, name.v, t, typ)
            case None => this.copy(typeMap = typeMap + (name -> typ))
        }
        override def get(name: Id): Option[Type] = typeMap.get(name)
        override def getMappedIds(): Set[Id] = typeMap.keySet
        override def intersect(other: TypeEnvironment): TypeEnvironment = {
            Env(
                getMappedIds.intersect(other.getMappedIds())
                        .filter( id => {this(id) == other(id) })
                        .map(k => { k -> this(k) } )
                        .toMap
            )
        }
    }
}
