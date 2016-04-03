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

object StandaloneSnippet {
  
  case class SnippetResult(name: String, result: Result, snippetText: String)
  
  def run(prms: Parameters = Parameters.default, props: Properties, snippet: (Result,Boolean) => Try[String], shrink: Boolean): Seq[SnippetResult] = {
    val params = props.overrideParameters(prms)
    for { 
      (name,p) <- props.properties
      p2 = Prop.collect(params)(p)
      res = Test.check(params, p2)
      snip = snippet(res, shrink) if(snip.isSuccess)
    } yield 
      SnippetResult(name,res,snip.get)
  }
}

object ScalaStatements {
  def packageStmt(s:String) = s"package ${s}"
  
  def importStmt(s:String) = s"import ${s}"
  
  def objStmt(name: String, body: String) = s"""
object ${name} {
${body}
}"""

  def mainStmt(contents: Seq[String], sep: String = "  ") = {
    val mainBody = contents.foldLeft(new StringBuilder()) {
      case (sb, l) => sb.append((sep*2) + l + "\n")
    }
    s"""
${sep}val failures = List(False, Exception, Undecided)

${sep}def main(args: Array[String]) = {
${mainBody}
${sep}}"""
  }
}



trait Yo {
  trait Apple {
    
  }
  
  case class George(s: String) extends Apple {
  }
}

trait RunnableSnippet extends Commands with Yo{
  import org.scalacheck.Test._
  
  val sep = "  "
  val objName = s"Snippet${scala.util.Random.nextInt(100000)}"
  val specName = this.getClass.getName.split('.').last.dropRight(1)
  val packageName = this.getClass.getName().split('.').dropRight(1).mkString(".")
  val imports = List("scala.util.Try", "org.scalacheck.commands._", s"${specName}._", "org.scalacheck.Prop.{False, Exception, Undecided}")
  
  /** Snippet for every command. **/
  def commandSnippet(i: Int): Seq[String] = Seq(
    s"val r${i} = Try(c${i}.run(sut, s${i}))",
    s"val p${i} = c${i}.postCondition(s${i}, r${i}).apply(org.scalacheck.Gen.Parameters.default).status",
    s"val t${i} = DynamicTerm(TermId(${i}), Some(r${i}))",
    s"val s${i+1} = c${i}.nextState(s${i}, t${i})",
    s"""println("${i} => " + c${i} + " = " + r${i})""",
    s"""println("\tc${i}.postCondition => " + p${i})""",
    "")

  def rewriteTerm[T](t: Term[T]): String = {
    if(t.isEmpty) s"SymbolicTerm(TermId(${t.id}))"
    else s"DynamicTerm(TermId(${t.id}), ${t.res.pp})"
  }
  
  def rewriteCommand(c: Command): String = {
    val p = c.asInstanceOf[Product]
    val args = MutableList[String]()
    
    for(ix <- 0 until p.productArity) {
      val elem = p.productElement(ix)
      if(elem.isInstanceOf[Term[_]]) {
        args += rewriteTerm(elem.asInstanceOf[Term[c.Result]])
      } else {
        args += ScalaPP.format(elem)
      }
    }
    
    s"${c.getClass.getSimpleName}(${args mkString(",")})"
  }
  
  def actionsSnippet(a: Actions): Seq[String] = {
    val (commands, commandsRun, _) = a.seqCmds.foldLeft((List[String](), List[String](), 0)) {
      case ((lst1, lst2, i), c: Command) => {
        (lst1 ++ List(s"val c${i} = new ${ScalaPP.format(c)}"), 
         lst2 ++ commandSnippet(i),
         i + 1)
      }
    }
    
    Seq(
        s"val s0 = ${specName}.${a.s}",
        s"val sut = ${specName}.newSut(s0)") ++ commands ++ commandsRun  
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
      case Proved(x::xs) => handleArg(x, shrink)
      case x => {
        Failure(new Exception(s"Irrelevant ${x} Test status"))
      }
    }
  }
}