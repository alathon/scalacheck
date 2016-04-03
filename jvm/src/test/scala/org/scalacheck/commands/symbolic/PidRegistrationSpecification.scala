package org.scalacheck.commands.symbolic

import org.scalacheck._
import org.scalacheck.commands._
import org.scalacheck.Gen._
import scala.util.{Success, Failure, Try}
import org.scalacheck.Properties
import scala.util.Failure
import org.scalacheck.Prop.propBoolean
import org.scalacheck.commands.Commands
import java.io.PrintWriter
import java.io.File

object CommandsPidRegistration extends Properties("CommandsPidRegistration") {
  import org.scalacheck.Test._
  
  property("pidregspec") = PidRegistrationSpecification.property()
  
  override def main(args: Array[String]): Unit = {
    val res = StandaloneSnippet.run(props = this, 
        snippet = PidRegistrationSpecification.snippet, 
        shrink = true)

    for {
      r <- res
      writer = new PrintWriter(new File(r.name + ".scala"))
      log = new PrintWriter(new File(r.name + ".log"))
    } yield {
      r.result.status match {
        case Failed(x::xs, labels) => {
          log.write(labels.toString) // TODO::: This is not making any sense.. Argh...
        }
        case _ =>
      }
      log.close()
      writer.write(r.snippetText)
      writer.close()
    }
  }
}

object PidRegistrationSpecification extends Commands  with RunnableSnippet {
  
  type Sut = PidSpawner

  case class State(
    pids: Set[Term[String]],
    regs: Map[String, Term[String]])

  override def genInitialState: Gen[State] = State(
    pids = Set.empty,
    regs = Map.empty)


  override def canCreateNewSut(newState: State, initSuts: Traversable[State],
                               runningSuts: Traversable[Sut]
                                ): Boolean = {
    initSuts.isEmpty && runningSuts.isEmpty
  }

  override def destroySut(sut: Sut): Unit = {
  }

  override def initialPreCondition(state: State): Boolean = true

  override def newSut(state: State): Sut = {
    new PidSpawner()
  }
  
  def genCommand(state: State): Gen[Command] = {
    frequency(
      (50, genSpawn),
      (20, genRegister(state)),
      (20, genUnregister(state)),
      (5, genWhereIsRandom),
      (20, genWhereIs(state)),
      (5, genUnregisterRandom),
      (20, genUnregister(state))
    )
  }

  def genSpawn: Gen[Spawn] = Spawn()

  def genRegister(state: State): Gen[Command] = {
    if(state.pids.isEmpty) genSpawn else for {
      t <- Gen.oneOf(state.pids.toSeq)
      name <- Gen.identifier
    } yield Register(t, name)
  }

  def genUnregisterRandom: Gen[Command] = {
    for {
      id <- identifier
    } yield Unregister(id)
  }

  def genUnregister(state: State): Gen[Command] = {
    if(state.regs.isEmpty) genUnregisterRandom else for {
      (name,_) <- oneOf(state.regs.toSeq)
    } yield Unregister(name)
  }

  def genWhereIsRandom: Gen[Command] = {
    for {
      id <- Gen.identifier
    } yield WhereIs(id)
  }

  def genWhereIs(state: State): Gen[Command] = {
    if(state.regs.isEmpty) genWhereIsRandom else for {
      (id,_) <- oneOf(state.regs.toSeq)
    } yield WhereIs(id)
  }

  case class Spawn() extends Command {
    override type Result = String

    override def preCondition(s: State): Boolean = true

    override def nextState(s: State, v:Term[Result]) = {
      s.copy(pids = s.pids + v)
    }

    override def postCondition(s: State, result: Try[Result]): Prop = {
      result.isSuccess
    }

    override def run(sut: Sut, s: State): Result = {
      sut.spawn()
    }
  }

  case class Register(pid: Term[String], name: String) extends Command {
    def findPid(s: State) = s.pids.find(_ == pid)
    
    override type Result = Unit
    
    override def preCondition(s: State): Boolean = findPid(s).isDefined
    
    override def nextState(s: State, v:Term[Result]) = {
      if(s.regs.contains(name)) {
        s
      } else {
        findPid(s) map(t => s.copy(regs = s.regs ++ Map(name -> t))) getOrElse s
      }
    }

    override def postCondition(s: State, result: Try[Result]): Prop = {
      if(!s.regs.contains(name)) result.isSuccess
      else result.isFailure
    }
    
    override def run(sut: Sut, s: State): Result = {
      for {
        term <- findPid(s)
        pidName <- term
      } yield sut.register(pidName, name)
    }
  }
  
  case class WhereIs(name: String) extends Command {
    override type Result = Option[String]

    override def preCondition(s: State): Boolean = true

    override def nextState(s: State, v:Term[Result]) = s

    override def postCondition(s: State, result: Try[Result]): Prop = {
      result match {
        case Success(sutRes) => {
          val modelRes = for {
            term <- s.regs.get(name)
            reg <- term
          } yield reg
          
          modelRes == sutRes
        }
        
        case Failure(e) => Prop.exception(e)
      }
    }

    override def run(sut: Sut, s: State): Result = {
      sut.whereis(name)
    }
  }
  
  case class Unregister(name: String) extends Command {
    override type Result = Unit

    def prep(s: State) = s.regs.contains(name)
    
    override def preCondition(state: State): Boolean = true

    override def nextState(s: State, v:Term[Result]) = {
      if(prep(s)) s.copy(regs = s.regs - name)
      else s
    }

    override def postCondition(s: State, result: Try[Result]): Prop = {
      if(result.isSuccess) prep(s)
      else !prep(s)
    }

    override def run(sut: Sut, s: State): Result = {
      sut.unregister(name)
    }
  }
}