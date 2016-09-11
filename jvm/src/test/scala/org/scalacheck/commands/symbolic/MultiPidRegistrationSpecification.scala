package org.scalacheck.commands.symbolic

import org.scalacheck.Properties
import org.scalacheck.Prop
import org.scalacheck.Gen
import org.scalacheck.Gen._
import scala.util.Try
import org.scalacheck.commands.StandaloneSnippet
import org.scalacheck.commands.RunnableSnippet
import org.scalacheck.commands.Commands

import org.scalacheck.Test.Failed
import org.scalacheck.Test.Exhausted
import org.scalacheck.Test.Passed
import org.scalacheck.Test.Proved

import org.scalacheck.Prop.propBoolean
import scala.language.postfixOps

import java.io.PrintWriter
import java.io.File

object CommandsMultiPidRegistration extends Properties("CommandsMultiPidRegistration") {
  property("multipidregspec") = MultiPidRegistrationSpecification.property(threadCount = 1)
  
  override def main(args: Array[String]): Unit = {
    val res = StandaloneSnippet.run(props = this, 
        snippet = MultiPidRegistrationSpecification.snippet, 
        shrink = true)
  
    for {
      r <- res
      writer = new PrintWriter(new File(r.name + ".scala"))
      log = new PrintWriter(new File(r.name + ".log"))
    } yield {
      r.result.status match {
        case Failed(x::xs, labels) => {
          println(labels toString)
          log.write(labels toString)
        }
        case _ =>
          println("Status: " + r.result.status.toString)
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
      (20, genUnregisterRandom),
      (5, genWhereIsRandom),
      (20, genWhereIs(state))
    )

  def genListPids: Gen[ListPids] = ListPids()
  
  def genRegister(state: State) = for {
      name <- Gen.identifier
    } yield Register(name)

  def genUnregister(state: State) =
    if(state.regs.isEmpty) genRegister(state) else for {
      (name,_) <- oneOf(state.regs.toSeq)
    } yield Unregister(name)

  def genUnregisterRandom = for {
    name <- Gen.identifier
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
    
    def regTaken(s: State): Boolean = s.regs.contains(name)
  }

  case class WhereIs(name: String) extends Command {
    override type Result = String

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