package org.scalacheck.commands

import scala.util.Try
import scala.util.Failure
import scala.util.Success
import org.scalacheck.Test.Parameters
import org.scalacheck.Properties
import org.scalacheck.Prop
import org.scalacheck.Prop.Arg
import org.scalacheck.Test.Result
import org.scalacheck.Test
import scala.collection.mutable.MutableList
import com.todesking.scalapp.syntax._
import com.todesking.scalapp.ScalaPP
import org.scalacheck.util.ConsoleReporter
import java.io.File
import java.io.PrintWriter
import org.scalacheck.Test.Failed

object StandaloneSnippet {
  case class SnippetResult(name: String, result: Result, snippetText: String)
  
  def dumpLog(r: SnippetResult, filename: String, console: Boolean = false) = {
    val writer = new PrintWriter(new File(filename))
    
    r.result.status match {
      case Failed(x::xs, labels) => {
        if(console) println(labels.toString)
        writer.write(labels.toString)
      }
      case _ =>
        if(console) println("Status: " + r.result.status.toString)
    }
    writer.close()
  }
  
  def dumpRunnable(s: SnippetResult, filename: String, console: Boolean = false) = {
    val writer = new PrintWriter(new File(filename))
    writer.write(s.snippetText)
    writer.close()
    if(console) println(s.snippetText)
  }

  def run(prms: Parameters = Parameters.default, props: Properties, snippet: (Result,Boolean) => Try[String], shrink: Boolean): Option[SnippetResult] = {
    val params = props.overrideParameters(prms).withTestCallback(ConsoleReporter(1))
    
    props.properties
      .collectFirst({ case p => p })
      .flatMap { case (name, prop) =>
        val p2 = Prop.collect(params)(prop)
        val res = Test.check(params, p2)
        val snip = snippet(res, shrink)
        snip.map(s => SnippetResult(name, res, s)).toOption
      }
  }
}

object ScalaStatements {
  
  def utils: String = s"""
  def cmdFormat(c: Command): String = ScalaPP.format(c) + (c.getMetadata map { m => " { Metadata: " + ScalaPP.format(m) + " }" } getOrElse "")
  def resultFormat[T](r: Try[T]): String = r.map(ScalaPP.format(_)).getOrElse(ScalaPP.format(r))
  val params = org.scalacheck.Gen.Parameters.default"""
  
  def commandSnippet(cmdIdx: Int, stateIdx: Int): Seq[String] = Seq(
    s"val r${cmdIdx} = Try(c${cmdIdx}.run(sut, s${stateIdx}))",
    s"val p${cmdIdx} = c${cmdIdx}.postCondition(s${stateIdx}, r${cmdIdx}).apply(params).status",
    s"val t${cmdIdx} = DynamicTerm(TermId(${cmdIdx}), Some(r${cmdIdx}))",
    s"val s${stateIdx+1} = c${cmdIdx}.nextState(s${stateIdx}, t${cmdIdx})",
    s"""println("c${cmdIdx} => " + cmdFormat(c${cmdIdx}) + " = " + resultFormat(r${cmdIdx}))""",
    s"""println("\tc${cmdIdx}.postCondition => " + p${cmdIdx})""",
    "")
    
  def packageStmt(s:String) = s"package ${s}"
  
  def importStmt(s:String) = s"import ${s}"
  
  def objStmt(name: String, body: String) = s"""
object ${name} {
${utils}
${body}
}"""

  def mainStmt(contents: Seq[String], sep: String = "  ") = {
    val mainBody = contents.foldLeft(new StringBuilder()) {
      case (sb, l) => sb.append((sep*2) + l + "\n")
    }
    s"""
${sep}def main(args: Array[String]) = {
${mainBody}
${sep}}"""
  }
}

trait RunnableSnippet extends Commands {
  import org.scalacheck.Test._
  
  val sep = "  "
  val objName = s"Snippet${scala.util.Random.nextInt(100000)}"
  val specName = this.getClass.getName.split('.').last.dropRight(1)
  val packageName = this.getClass.getName().split('.').dropRight(1).mkString(".")
  val imports = List("scala.util.Try", "org.scalacheck.commands._", s"${specName}._", "com.todesking.scalapp.ScalaPP")
  
  def commandInit(c: Command, idx: Int): String = {
    val metadata = c.getMetadata map { m => s"{ override def genMetadata(s: State) = ${ScalaPP.format(m)} }" } getOrElse ""
    s"val c${idx} = new ${ScalaPP.format(c)} ${metadata}" 
  }
  
  def actionsSnippet(a: Actions): Seq[String] = {
    val (commands, commandsRun, _) = a.seqCmds.zip(a.seqTerms).foldLeft((List[String](), List[String](), 0)) {
      case ((lst1, lst2, stateIdx), (c,t)) => {
        (lst1 ++ List(commandInit(c, t.id.id)),
         lst2 ++ ScalaStatements.commandSnippet(t.id.id, stateIdx),
         stateIdx + 1)
      }
    }

    Seq(
        s"val s0 = ${specName}.${a.s}",
        s"val sut = ${specName}.newSut(s0)") ++ Seq("") ++ commands ++ Seq("") ++ commandsRun  
    }

  /* All this .isInstanceOf / .asInstanceOf is pretty horrible.
   * Isn't there a better way of dealing with type erasure???
   */
  def handleArg(arg: Arg[Any], shrink: Boolean): Try[String] = {
    if(!arg.arg.isInstanceOf[Actions]) 
      Failure(throw new Exception("RunnableSnippet is only for Stateful testing in ScalaCheck."))

    val actions:Actions = if(shrink) arg.arg.asInstanceOf[Actions] else arg.origArg.asInstanceOf[Actions]
    val contents = ScalaStatements.mainStmt(actionsSnippet(actions))
    Success(s"""
${ScalaStatements.packageStmt(packageName)}

${imports map(ScalaStatements.importStmt) mkString "\n"}

${ScalaStatements.objStmt(objName, contents)}
""")
  }
  
  def snippet(r: Result, shrink: Boolean): Try[String] = {
    r.status match {
      case Failed(x::xs,_) => handleArg(x, shrink)
      //case Proved(x::xs) => handleArg(x, shrink)
      case x => {
        Failure(new Exception(s"Irrelevant ${x} Test status"))
      }
    }
  }
}