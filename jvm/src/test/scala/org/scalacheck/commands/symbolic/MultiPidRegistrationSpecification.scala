package org.scalacheck.commands.symbolic

import org.scalacheck._
import org.scalacheck.commands._
import org.scalacheck.Gen._
import scala.util.Try
import org.scalacheck.Prop.propBoolean
import org.scalacheck.commands.Commands
import scala.language.postfixOps

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
  
  def genCommand(state: State): Gen[Command] = frequency(
      (50, genListPids),
      (20, genRegister(state)),
      (20, genUnregister(state)),
      (5, genWhereIsRandom),
      (20, genWhereIs(state)),
      (5, genUnregisterRandom),
      (20, genUnregister(state))
    )

  def genListPids: Gen[ListPids] = ListPids()
  
  def genRegister(state: State) = for {
      name <- Gen.identifier
    } yield Register(name)

  def genUnregisterRandom = for {
      id <- identifier
    } yield Unregister(id)

  def genUnregister(state: State) =
    if(state.regs.isEmpty) genUnregisterRandom else for {
      (name,_) <- oneOf(state.regs.toSeq)
    } yield Unregister(name)

  def genWhereIsRandom = for {
      id <- Gen.identifier
    } yield WhereIs(id)

  def genWhereIs(state: State) =
    if(state.regs.isEmpty) genWhereIsRandom else for {
      (id,_) <- oneOf(state.regs.toSeq)
    } yield WhereIs(id)
  
  def getPid(idx: Int, s: State): Option[String] = {
      for {
        term <- s.pids
        pids <- term
      } yield pids.lift(idx)
    } flatten

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
    
    var idx:Option[Int] = None
    
    override def preCondition(state: State) = true
    
    override def run(sut: Sut, s: State): Result = {
      val maybePid = s.pids flatMap { term =>
        for {
          i <- getIdx(s)
          pids <- term
        } yield pids.lift(i)
      } flatten
      
      sut.register(maybePid getOrElse "Invalid PID", name)
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

    def regTaken(s: State): Boolean = {
      val toReg = idx map { getPid(_, s) } flatten

      s.regs.exists { case (name2, term) =>
        name == name2 || term.toOption == toReg
      }
    }

    // TODO: This is pretty horrible. Also, this
    // doesn't survive past shrinking, since
    // 'idx' is nowhere to be found wrt. State/etc.
    // Also-also: This is an uncontrolled, bad way to do side-effects
    def getIdx(state: State): Option[Int] = {
      idx orElse {
        for {
          term <- state.pids
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