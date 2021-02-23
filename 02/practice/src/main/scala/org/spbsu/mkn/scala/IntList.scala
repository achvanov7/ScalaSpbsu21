package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.IntList._

sealed trait IntList {
  def head: Int

  def tail: IntList

  def drop(n: Int): IntList

  def take(n: Int): IntList

  def map(f: Int => Int): IntList

  def ::(elem: Int): IntList = new ::(elem, this)
}

object IntList {
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")

  def fromSeq(seq: Seq[Int]): IntList = seq.foldRight[IntList](IntNil)((v: Int, acc: IntList) => v :: acc)

  def sum(intList: IntList): Int = intList match {
    case IntNil => undef
    case xs => foldLeft((acc: Int, x: Int) => acc + x)(0)(xs)
  }

  def size(intList: IntList): Int = foldLeft((acc: Int, _: Int) => acc + 1)(0)(intList)

  def foldLeft(f: (Int, Int) => Int)(init: Int): IntList => Int = {
    case IntNil => init
    case head :: tail => foldLeft(f)(f(init, head))(tail)
  }
}

case object IntNil extends IntList {
  override def head: Int = undef

  override def tail: IntList = undef

  override def drop(n: Int): IntList = if (n == 0) this else undef

  override def take(n: Int): IntList = if (n == 0) this else undef

  override def map(f: Int => Int): IntList = this
}

case class ::(override val head: Int, override val tail: IntList) extends IntList {
  override def drop(n: Int): IntList = if (n == 0) this else tail.drop(n - 1)

  override def take(n: Int): IntList = if (n == 0) IntNil else head :: tail.take(n - 1)

  override def map(f: Int => Int): IntList = f(head) :: tail.map(f)
}
