package org.scalacheck.commands.symbolic

class PidSpawner() {
  case class PidSpawnerState(
    pids: Set[String],
    regs: Map[String, String])

  var state = PidSpawnerState(pids = Set.empty, regs = Map.empty)

  def genUUID() = java.util.UUID.randomUUID.toString

  def spawn():String = {
    val uuid = genUUID()
    state = state.copy(pids = state.pids + uuid)
    uuid
  }

  def register(uuid: String, name: String): Unit = {
    if(state.pids.exists(x => x == uuid) && !state.regs.exists(x => x._1 == name)) {
      //state = state.copy(regs = state.regs ++ Map(name -> uuid))
    } else {
      throw new Exception(s"Unable to register ${name} -> ${uuid}.")
    }
  }

  def unregister(name: String): Unit = {
    if(state.regs.contains(name)) {
      state = state.copy(regs = state.regs - name)
    } else {
      throw new Exception(s"Unable to unregister ${name}")
    }
  }

  def whereis(name: String): Option[String] = {
    state.regs.find(_._1 == name).map { case (n,u) => u }
  }
}