
package org.scalacheck.commands.symbolic

import scala.util.Try
import org.scalacheck.commands._
import PidRegistrationSpecification._
import com.todesking.scalapp.ScalaPP


object Snippet1485 {

  def cmdFormat(c: Command): String = ScalaPP.format(c) + (c.getMetadata map { m => " { Metadata: " + ScalaPP.format(m) + " }" } getOrElse "")
  def resultFormat[T](r: Try[T]): String = r.map(ScalaPP.format(_)).getOrElse(ScalaPP.format(r))
  val params = org.scalacheck.Gen.Parameters.default

  def main(args: Array[String]) = {
    val s0 = PidRegistrationSpecification.State(Set(),Map())
    val sut = PidRegistrationSpecification.newSut(s0)
    
    val c2 = new Spawn() 
    val c4 = new Register(Term(2), "rumhb") 
    val c5 = new Unregister("rumhb") 
    
    val r2 = Try(c2.run(sut, s0))
    val p2 = c2.postCondition(s0, r2).apply(params).status
    val t2 = DynamicTerm(TermId(2), Some(r2))
    val s1 = c2.nextState(s0, t2)
    println("c2 => " + cmdFormat(c2) + " = " + resultFormat(r2))
    println("	c2.postCondition => " + p2)
    
    val r4 = Try(c4.run(sut, s1))
    val p4 = c4.postCondition(s1, r4).apply(params).status
    val t4 = DynamicTerm(TermId(4), Some(r4))
    val s2 = c4.nextState(s1, t4)
    println("c4 => " + cmdFormat(c4) + " = " + resultFormat(r4))
    println("	c4.postCondition => " + p4)
    
    val r5 = Try(c5.run(sut, s2))
    val p5 = c5.postCondition(s2, r5).apply(params).status
    val t5 = DynamicTerm(TermId(5), Some(r5))
    val s3 = c5.nextState(s2, t5)
    println("c5 => " + cmdFormat(c5) + " = " + resultFormat(r5))
    println("	c5.postCondition => " + p5)
    

  }
}
