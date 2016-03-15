package org.scalacheck.commands

import scala.util.Random
import org.scalacheck._
import org.scalacheck.commands._
import org.scalacheck.Gen._
import scala.util.{Success, Failure, Try}
import java.io.PrintWriter
import java.io.File
import scala.language.postfixOps

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

  case class State(
    pids: Option[Term[Seq[String]]],
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
    if(state.pids.isEmpty) genListPids else for {
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
  
  def getPid(idx: Int, state: State) = for {
    pids <- state.pids.flatMap(_.map(identity))
  } yield pids.lift(idx)
  
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
  
  
  case class Register(name: String) extends Command {
    override type Result = String
    
    override def preCondition(state: State) = state.pids.isDefined
    
    override def run(sut: Sut, s: State): Result = {
      {
        for {
          pids <- s.pids.flatMap(_.map(identity))
          idx = getIdx(pids)
          pid = pids(idx)
        } yield {
          sut.register(pid, name)
          pid
        }
      } getOrElse {
        throw new Exception("preCondition probably violated.")
      }
    }

    override def postCondition(s: State, result: Try[Result]): Prop = {
      if(regTaken(s)) result.isFailure
      else result.isSuccess
    }
    
    override def nextState(s: State, v:Term[Result]) = {
      if(regTaken(s)) {
        s
      } else {
        s.copy(regs = s.regs ++ Map(name -> v))
      }
    }
    
    var pidIdx:Option[Int] = None
    
    def getIdx(pids: Seq[_]): Int = {
      pidIdx getOrElse {
        val idx = scala.util.Random.nextInt(pids.size)
        pidIdx = Some(idx)
        idx
      }
    }
    
    def regTaken(s: State): Boolean = {
      val maybePid = pidIdx flatMap { getPid(_, s) } flatten
      
      val entries = for {
        (n, t) <- s.regs
        pid <- maybePid
      } yield (name == n || t.exists(_ == pid))
      
      entries.exists(_ == true)
    }
  }

  case class WhereIs(name: String) extends Command {
    override type Result = Option[String]

    override def preCondition(s: State): Boolean = s.pids.isDefined

    override def nextState(s: State, v:Term[Result]) = s
    
    override def postCondition(s: State, result: Try[Result]): Prop = {
      result match {
        case Failure(e) => Prop.exception(e)
        
        case Success(pid1) => {
          val pid2 = for {
            (_, term) <- s.regs.find(_._1 == name)
            value <- term
          } yield value

          pid1 == pid2
        }
      }
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
      val pid = for {
        (_, term) <- s.regs.find(_._1 == name)
        value <- term
      } yield value
      
      if(pid.isEmpty) result.isFailure
      else result.isSuccess
    }

    override def run(sut: Sut, s: State): Result = {
      sut.unregister(name)
    }
  }
}