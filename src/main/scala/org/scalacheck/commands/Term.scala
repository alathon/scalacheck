package org.scalacheck.commands

import scala.util.Try

abstract sealed class Term[+A, C](c: C) extends Product with Serializable {
  val binding = new AnyRef()
  override def equals(o: Any) = o.isInstanceOf[Term[A,C]] && 
    o.asInstanceOf[Term[A,C]].binding == this.binding
      
   def isEmpty: Boolean
   def get: A
    
   final def flatMap[B](f: A => Term[B,C]): Term[B,C] =
     if(isEmpty) SymbolicTerm(c) else f(this.get)
   
   final def map[B](f: A => B): Term[B,C] = if(isEmpty) SymbolicTerm(c) else DynamicTerm(c, Try(f(this.get)))
   
   final def getOrElse[B >: A](default: => B): B =
     if(isEmpty) default else this.get
   
   final def fold[B](ifEmpty: => B)(f: A => B): B =
     if(isEmpty) ifEmpty else f(this.get)
     
   def flatten[B](implicit ev: A <:< Term[B,C]): Term[B,C] =
     if(isEmpty) SymbolicTerm(c) else ev(this.get)
   
   final def filter(p: A => Boolean): Option[A] =
     if(isEmpty || !p(this.get)) None else Some(this.get)
  }
  
  sealed case class SymbolicTerm[+A,C](c: C) extends Term[Nothing,C](c) {
    override def get = throw new Exception("Symbolic terms have no value.")
    override def isEmpty = true
  }
  
  sealed case class DynamicTerm[+A,C](c: C, res: Try[A]) extends Term[A,C](c) {
    override def get = res.get
    override def isEmpty = false
  }