package pipedsl.common

import pipedsl.common.Locks.LockType
import pipedsl.common.Syntax.{Id, Prog}

//A holder class for metadata about programs that we want to pass between stages, etc.
//TODO move other metadata here from the various random ways that it's currently stored.
class ProgInfo(val p: Prog) {

  private val modInfo: Map[Id, ModInfo] = p.moddefs.foldLeft(Map[Id, ModInfo]())((map, mod) => {
    map + (mod.name -> new ModInfo())
  })

  def getModInfo(mod: Id): ModInfo = {
    modInfo(mod)
  }

  def addLockInfo(locktypeinfo: Map[Id, Map[Id, LockType]]): Unit = {
    locktypeinfo.keys.foreach(m => {
      modInfo(m).setLockTypes(locktypeinfo(m))
    })
  }

  class ModInfo {

    private var lockType: Map[Id, LockType] = Map()

    def setLockTypes(lockTypes: Map[Id, LockType]): Unit = {
      lockType = lockTypes
    }
    def getLockTypes: Map[Id, LockType] = lockType
  }

}
