
package org.scalacheck.commands.symbolic

import scala.util.Try
import org.scalacheck.commands._
import MultiPidRegistrationSpecification2._
import com.todesking.scalapp.ScalaPP
import scala.util.Success


object Snippet57520 {

  def cmdFormat(c: Command): String = ScalaPP.format(c) + (c.getMetadata map { m => " { Metadata: " + ScalaPP.format(m) + " }" } getOrElse "")
  def resultFormat[T](r: Try[T]): String = r.map(ScalaPP.format(_)).getOrElse(ScalaPP.format(r))
  val params = org.scalacheck.Gen.Parameters.default
  
  def main(args: Array[String]) = {
    def examine[T](t: Term[T]) = t match {
    case SymbolicTerm(id) => println("Symbolic Term with ID: " + id)
    case DynamicTerm(id, v) => println("Dynamic Term with ID: " + id + " and value: " + v)
  }
  
  val tst = SymbolicTerm[String](TermId(5))
  val tst2 = DynamicTerm[String](TermId(3), Some(Success("Hello World")))
  examine(tst)
  examine(tst2)
  
    val s0 = MultiPidRegistrationSpecification2.State(None,List(),Map())
    val sut = MultiPidRegistrationSpecification2.newSut(s0)
    
    val c0 = new ListPids() 
    val c1 = new GetPid() { override def genMetadata(s: State) = Some(5) }
    val c9 = new Register(Term(1), "x") 
    val c10 = new Unregister("x") 
    val c20 = new GetPid() { override def genMetadata(s: State) = Some(24) }
    val c22 = new Register(Term(20), "x") 
    
    val r0 = Try(c0.run(sut, s0))
    val p0 = c0.postCondition(s0, r0).apply(params).status
    val t0 = DynamicTerm(TermId(0), Some(r0))
    val s1 = c0.nextState(s0, t0)
    println("c0 => " + cmdFormat(c0) + " = " + resultFormat(r0))
    println("	c0.postCondition => " + p0)
    
    val r1 = Try(c1.run(sut, s1))
    val p1 = c1.postCondition(s1, r1).apply(org.scalacheck.Gen.Parameters.default).status
    val t1 = DynamicTerm(TermId(1), Some(r1))
    val s2 = c1.nextState(s1, t1)
    println("c1 => " + cmdFormat(c1) + " = " + resultFormat(r1))
    println("	c1.postCondition => " + p1)
    
    val r9 = Try(c9.run(sut, s2))
    val p9 = c9.postCondition(s2, r9).apply(org.scalacheck.Gen.Parameters.default).status
    val t9 = DynamicTerm(TermId(9), Some(r9))
    val s3 = c9.nextState(s2, t9)
    println("c9 => " + cmdFormat(c9) + " = " + resultFormat(r9))
    println("	c9.postCondition => " + p9)
    
    val r10 = Try(c10.run(sut, s3))
    val p10 = c10.postCondition(s3, r10).apply(org.scalacheck.Gen.Parameters.default).status
    val t10 = DynamicTerm(TermId(10), Some(r10))
    val s4 = c10.nextState(s3, t10)
    println("c10 => " + cmdFormat(c10) + " = " + resultFormat(r10))
    println("	c10.postCondition => " + p10)
    
    val r20 = Try(c20.run(sut, s4))
    val p20 = c20.postCondition(s4, r20).apply(org.scalacheck.Gen.Parameters.default).status
    val t20 = DynamicTerm(TermId(20), Some(r20))
    val s5 = c20.nextState(s4, t20)
    println("c20 => " + cmdFormat(c20) + " = " + resultFormat(r20))
    println("	c20.postCondition => " + p20)
    
    val r22 = Try(c22.run(sut, s5))
    val p22 = c22.postCondition(s5, r22).apply(org.scalacheck.Gen.Parameters.default).status
    val t22 = DynamicTerm(TermId(22), Some(r22))
    val s6 = c22.nextState(s5, t22)
    println("c22 => " + cmdFormat(c22) + " = " + resultFormat(r22))
    println("	c22.postCondition => " + p22)
    

  }
}
