package org.scalacheck.commands.symbolic

class MultiPidSpawner() {
  case class MultiPidSpawnerState(
    pids: Seq[String],
    regs: Map[String, String])

  private val uuids:Seq[String] =  Seq.fill(30)(genUUID())
    
  var state = MultiPidSpawnerState(pids = uuids, regs = Map.empty)

  def genUUID() = java.util.UUID.randomUUID.toString

  def listPids(): Seq[String] = state.pids

  def register(uuid: String, name: String): String = {
    if(state.pids.exists(x => x == uuid)) {
      if(!state.regs.exists(x => x._1 == name)) {
        state = state.copy(regs = state.regs ++ Map(name -> uuid))
        return uuid
      } else {
        throw new Exception("Cannot register the same name twice.")
      }
    } else {
      throw new Exception("No such PID as " + uuid)
    }
  }

  def unregister(name: String): Unit = {
    state.regs.find(_._1 == name) map {
      case _ => {
        //state = state.copy(regs = state.regs - name)
      }
    } getOrElse (throw new Exception("No such registration as " + name))
  }

  def whereis(name: String): String = {
    state.regs.find(_._1 == name).map { case (n,u) => u } getOrElse 
      (throw new Exception("No registration by name " + name))
  }
}