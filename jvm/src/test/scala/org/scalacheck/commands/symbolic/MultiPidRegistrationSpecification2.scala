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
import com.todesking.scalapp.syntax._
import com.todesking.scalapp.ScalaPP
import scala.util.Failure

object CommandsMultiPidRegistration2 extends Properties("CommandsMultiPidRegistration2") {
  property("multipidspec2") = MultiPidRegistrationSpecification2.property(threadCount = 1)
  
  override def main(args: Array[String]): Unit = {
    val res = StandaloneSnippet.run(props = this, 
        snippet = MultiPidRegistrationSpecification2.snippet, 
        shrink = true)
  
    for {
      r <- res
      writer = new PrintWriter(new File(r.name + ".scala"))
      log = new PrintWriter(new File(r.name + ".log"))
    } yield {
      r.result.status match {
        case Failed(x::xs, labels) => {
          println(labels)
          log.write(labels toString)
        }
        case _ =>
      }
      log.close()
      writer.write(r.snippetText)
      writer.close()
    }
  }
  
}

object MultiPidRegistrationSpecification2 extends Commands with RunnableSnippet {
  
  type Sut = MultiPidSpawner
  
  case class State(
    listOfPids: Option[Term[Seq[String]]],
    pids: Seq[Term[String]],
    regs: Map[String, Term[String]])

  override def genInitialState: Gen[State] = State(listOfPids = None, pids = Seq.empty, regs = Map.empty)

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
      (1, genListPids),
      (5, genGetPid(state)),
      (20, genRegister(state)),
      (20, genUnregister(state)),
      (5, genWhereIs(state))
    )

  def genListPids: Gen[Command] = ListPids()
  
  def genGetPid(state: State): Gen[Command] =
    if(state.listOfPids.isEmpty) genListPids 
    else GetPid()

  def genRegister(state: State): Gen[Command] = 
    if(state.pids.isEmpty) genGetPid(state) else for {
      name <- Gen.resize(3, identifier)
      pid <- oneOf(state.pids.toSeq)
    } yield Register(pid, name)

  def genUnregister(state: State): Gen[Command] =
    if(state.regs.isEmpty) genGetPid(state) else for {
      (name,_) <- oneOf(state.regs.toSeq)
    } yield Unregister(name)

  def genWhereIs(state: State): Gen[Command] =
    if(state.regs.isEmpty) genGetPid(state) else for {
      (id,_) <- oneOf(state.regs.toSeq)
    } yield WhereIs(id)
  
  case class ListPids() extends Command {
    override type Result = Seq[String]
    
    override def preCondition(s: State): Boolean = true
    
    override def nextState(s: State, v:Term[Result]) = {
      s.copy(listOfPids = Option(v))
    }
    
    override def postCondition(s: State, result: Try[Result]): Prop = {
      result.isSuccess
    }
    
    override def run(sut: Sut, s: State): Result = {
      sut.listPids()
    }
  }

  case class Register(pid: Term[String], name: String) extends Command {
    override type Result = String
    
    override def preCondition(state: State) = state.pids.exists(_.id == pid.id)
    
    override def run(sut: Sut, s: State): Result = {
      pid.resolve(s.pids) map { p =>
        sut.register(p, name)
      } getOrElse(throw new Exception("Failed to resolve pid " + pid.id))
    }

    override def postCondition(s: State, result: Try[Result]): Prop = {
      if(regTaken(s)) result.isFailure
      else result.isSuccess
    }
    
    override def nextState(s: State, v:Term[Result]) = {
      if(regTaken(s)) s
      else s.copy(regs = s.regs ++ Map(name -> v))
    }
    
    def regTaken(s: State): Boolean = s.regs.exists { case (name2, _) => name == name2 }
  }

  case class GetPid() extends Command {
    override type Metadata = Option[String]
    
    override def genMetadata(s: State): Metadata = {
      for {
        TermResult(pids) <- s.listOfPids
        i = scala.util.Random.nextInt(pids.size)
      } yield pids(i)
    }
    
    override type Result = String
    
    override def preCondition(s: State): Boolean = s.listOfPids.isDefined
    
    override def nextState(s: State, v:Term[Result]) = s.copy(pids = s.pids ++ Seq(v))
    
    override def postCondition(s: State, result: Try[Result]): Prop = true
    
    override def run(sut: Sut, s: State): Result = {
      val pid = maybeSetMetadata(s)
      pid.get
    }
  }

  case class WhereIs(name: String) extends Command {
    override type Result = String

    override def preCondition(s: State): Boolean = true

    override def nextState(s: State, v:Term[Result]) = s
    
    override def postCondition(s: State, result: Try[Result]): Prop = {
      result match {
        case Failure(_) => !s.regs.exists(_._1 == name)
        case Success(pid) => {
          s.regs.find { 
            case (regName, TermResult(regPid)) => regName == name && regPid == pid 
          } isDefined
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
      if(s.regs.exists(_._1 == name)) result.isSuccess
      else result.isFailure
    }

    override def run(sut: Sut, s: State): Result = {
      sut.unregister(name)
    }
  }
}