package org.scalacheck.commands

import scala.util.Random
import org.scalacheck._
import org.scalacheck.commands._
import org.scalacheck.Gen._
import scala.util.{Success, Failure, Try}
import java.io.PrintWriter
import java.io.File
import scala.language.postfixOps
import scala.language.implicitConversions

class MultiPidSpawner() {
  case class MultiPidSpawnerState(
    pids: Seq[String],
    regs: Map[String, String])

  private val uuids:Seq[String] =  Seq.fill(30)(genUUID())
    
  var state = MultiPidSpawnerState(pids = uuids, regs = Map.empty)

  def genUUID() = java.util.UUID.randomUUID.toString

  def listPids(): Seq[String] = state.pids

  def register(uuid: String, name: String): Unit = {
    if(state.pids.exists(x => x == uuid)) {
      if(!state.regs.exists(x => x._1 == name || x._2 == uuid)) {
        state = state.copy(regs = state.regs ++ Map(name -> uuid))
      } else {
        throw new Exception("Cannot register a UUID twice.")
      }
    } else {
      throw new Exception("No PIDs!")
    }
  }

  def unregister(name: String): Unit = {
    state.regs.find(_._1 == name) map {
      case _ => {
        state = state.copy(regs = state.regs - name)
      }
    } getOrElse (throw new Exception("No such registration as " + name))
  }

  def whereis(name: String): Option[String] = {
    state.regs.find(_._1 == name).map { case (n,u) => u }
  }
}

object CommandsMultiPidRegistration extends Properties("CommandsMultiPidRegistration") {
  property("multipidregspec") = MultiPidRegistrationSpecification.property(threadCount = 1)
}

object MultiPidRegistrationSpecification extends Commands {
  
  type Sut = MultiPidSpawner

  type Pids = Seq[String]
  type PidIdx = Int
  
  case class State(
    pids: Option[Term[Pids]],
    regs: Map[String, Term[String]])

  override def genInitialState: Gen[State] = State(pids = None, regs = Map.empty)

  override def canCreateNewSut(newState: State, initSuts: Traversable[State],
                               runningSuts: Traversable[Sut]
                                ): Boolean = {
    initSuts.isEmpty && runningSuts.isEmpty
  }

  override def destroySut(sut: Sut): Unit = {
  }

  override def initialPreCondition(state: State): Boolean = true

  override def newSut(state: State): Sut = {
    new MultiPidSpawner()
  }
  
  def genCommand(state: State): Gen[Command] = {
    frequency(
      (50, genListPids),
      (20, genRegister(state)),
      (20, genUnregister(state)),
      (5, genWhereIsRandom),
      (20, genWhereIs(state)),
      (5, genUnregisterRandom),
      (20, genUnregister(state))
    )
  }

  def genListPids: Gen[ListPids] = ListPids()
  
  def genRegister(state: State) = {
    for {
      name <- Gen.identifier
    } yield Register(name)
  }

  def genUnregisterRandom = {
    for {
      id <- identifier
    } yield Unregister(id)
  }

  def genUnregister(state: State) = {
    if(state.regs.isEmpty) genUnregisterRandom else for {
      (name,_) <- oneOf(state.regs.toSeq)
    } yield Unregister(name)
  }

  def genWhereIsRandom = {
    for {
      id <- Gen.identifier
    } yield WhereIs(id)
  }

  def genWhereIs(state: State) = {
    if(state.regs.isEmpty) genWhereIsRandom else for {
      (id,_) <- oneOf(state.regs.toSeq)
    } yield WhereIs(id)
  }
  
  def getPid(idx: Int, s: State): Option[String] = {
    {
      for {
        term <- s.pids
        pids <- term
      } yield pids.lift(idx)
    } flatten
  }

  case class ListPids() extends Command {
    override type Result = Seq[String]

    override def preCondition(s: State): Boolean = true

    override def nextState(s: State, v:Term[Result]) = {
      s.copy(pids = Option(v))
    }

    override def postCondition(s: State, result: Try[Result]): Prop = {
      result.isSuccess
    }

    override def run(sut: Sut, s: State): Result = {
      sut.listPids()
    }
  }
  
  // Things to account for:
  // 1. No pids yet.
  // 2. Convert Option[Int] to Int (pids index).
  // 2.1. Generate random Int between 0 - pids.size if hasn't occured before.
  // 3. Look up the PID we want to register by index.
  case class Register(name: String) extends Command {
    override type Result = String
    
    override def preCondition(state: State) = true
    
    override def run(sut: Sut, s: State): Result = {
      val pid = {
        for {
          pids <- s.pids
          idx <- getIdx(pids)
          p <- getPid(idx, s)
        } yield p
      } getOrElse "Invalid PID"

      sut.register(pid, name)
      pid
    }

    override def postCondition(s: State, result: Try[Result]): Prop = {
      if(regTaken(s) || s.pids.isEmpty) result.isFailure
      else result.isSuccess
    }
    
    override def nextState(s: State, v:Term[Result]) = {
      if(regTaken(s) || s.pids.isEmpty) {
        s
      } else {
        s.copy(regs = s.regs ++ Map(name -> v))
      }
    }

    /* A registration is taken if either the PID is taken, or the name is taken. */
    def regTaken(s: State): Boolean = {
      val toReg = idx map { getPid(_, s) } flatten
      
      s.regs.exists { case (name2, term) =>
        name == name2 || (toReg.isDefined && term.exists(_ == toReg.get))
      }
    }
    
    var idx:Option[PidIdx] = None

    // I really don't like having to write code like this!
    // TODO: Find a better way to carry information between run()'s.
    def getIdx(term: Term[Pids]): Option[PidIdx] = {
      idx orElse {
        for {
          pids <- term
          i = scala.util.Random.nextInt(pids.size)
        } yield {
          idx = Some(i)
          i
        }
      }
    }
  }

  case class WhereIs(name: String) extends Command {
    override type Result = Option[String]

    override def preCondition(s: State): Boolean = s.pids.isDefined

    override def nextState(s: State, v:Term[Result]) = s
    
    override def postCondition(s: State, result: Try[Result]): Prop = {
      result map { pid1 =>
        val pid2 = for {
          (_, term) <- s.regs.find(_._1 == name)
          pid <- term
        } yield pid
        
        pid1 == pid2
      } isSuccess
    }

    override def run(sut: Sut, s: State): Result = {
      sut.whereis(name)
    }
  }

  case class Unregister(name: String) extends Command {
    override type Result = Unit

    override def preCondition(state: State): Boolean = true

    override def nextState(s: State, v:Term[Result]) = {
      if(s.regs.contains(name)) s.copy(regs = s.regs - name)
      else s
    }

    override def postCondition(s: State, result: Try[Result]): Prop = {
      if(s.regs.exists(_._1 == name)) result.isSuccess
      else result.isFailure
    }

    override def run(sut: Sut, s: State): Result = {
      sut.unregister(name)
    }
  }
}