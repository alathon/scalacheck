package org.scalacheck.commands.symbolic

import org.scalacheck._
import org.scalacheck.Test.Failed
import org.scalacheck.commands._
import org.scalacheck.Gen._
import scala.util.Try
import org.scalacheck.Prop.propBoolean
import org.scalacheck.commands.Commands
import scala.language.postfixOps
import scala.util.Success
import java.io.PrintWriter
import java.io.File

object CommandsMultiPidRegistration extends Properties("CommandsMultiPidRegistration") {
  property("multipidregspec") = MultiPidRegistrationSpecification.property(threadCount = 1)
  
  override def main(args: Array[String]): Unit = {
    val res = StandaloneSnippet.run(props = this, 
        snippet = MultiPidRegistrationSpecification.snippet, 
        shrink = false)
  
    for {
      r <- res if r.result.passed == false
      writer = new PrintWriter(new File(r.name + ".scala"))
      log = new PrintWriter(new File(r.name + ".log"))
    } yield {
      r.result.status match {
        case Failed(x::xs, labels) => {
          //log.write(labels(0) toString) // TODO::: This is not making any sense.. Argh...
        }
        case _ =>
      }
      log.close()
      writer.write(r.snippetText)
      writer.close()
    }
  }
}

object MultiPidRegistrationSpecification extends Commands with RunnableSnippet {
  
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
        TermResult(pids) <- s.pids
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
    override type Metadata = Option[Int]
    
    override def genMetadata(s: State): Metadata = {
      for {
        TermResult(pids) <- s.pids
        i = scala.util.Random.nextInt(pids.size)
      } yield i
    }
    
    override type Result = String
    
    override def preCondition(state: State) = true
    
    override def run(sut: Sut, s: State): Result = {
      val maybePid = {
        for {
          TermResult(pids) <- s.pids
          i <- maybeSetMetadata(s)
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
        val toReg = (getMetadata flatten) map { getPid(_, s) } flatten

      s.regs.exists { 
        case (name2, TermResult(reg)) =>
          name == name2 || Some(reg) == toReg
        case (name2, _) => name == name2
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
          (_, TermResult(pid)) <- s.regs.find(_._1 == name)
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