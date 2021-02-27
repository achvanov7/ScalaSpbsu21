package org.spbsu.mkn.scala

import org.spbsu.mkn.scala.MyGenericList._

sealed trait MyGenericList[+T] {
  def head: T

  def tail: MyGenericList[T]

  def drop(n: Int): MyGenericList[T]

  def take(n: Int): MyGenericList[T]

  def map[R](f: T => R): MyGenericList[R]

  def ::[NT >: T](elem: NT): MyGenericList[NT] = new ::(elem, this)
}

object MyGenericList {
  def undef: Nothing = throw new UnsupportedOperationException("operation is undefined")

  def fromSeq[T](seq: Seq[T]): MyGenericList[T] =
    seq.foldRight[MyGenericList[T]](MyNil)((v: T, acc: MyGenericList[T]) => v :: acc)

  def size[T](myList: MyGenericList[T]): Int = foldLeft((acc: Int, _: T) => acc + 1)(0)(myList)

  def sum(myList: MyGenericList[Int]): Int = myList match {
    case MyNil => undef
    case xs => foldLeft((acc: Int, x: Int) => acc + x)(0)(xs)
  }

  def foldLeft[T, R](f: (T, R) => T)(init: T): MyGenericList[R] => T = {
    case MyNil => init
    case head :: tail => foldLeft(f)(f(init, head))(tail)
  }
}

object MyNil extends MyGenericList[Nothing] {
  override def head: Nothing = undef

  override def tail: MyGenericList[Nothing] = undef

  override def drop(n: Int): MyGenericList[Nothing] = if (n == 0) this else undef

  override def take(n: Int): MyGenericList[Nothing] = if (n == 0) this else undef

  override def map[R](f: Nothing => R): MyGenericList[Nothing] = this
}

case class ::[T](override val head: T, override val tail: MyGenericList[T]) extends MyGenericList[T] {
  override def drop(n: Int): MyGenericList[T] = if (n < 0) undef else if (n == 0) this else tail.drop(n - 1)

  override def take(n: Int): MyGenericList[T] = if (n < 0) undef else if (n == 0) MyNil else head :: tail.take(n - 1)

  override def map[R](f: T => R): MyGenericList[R] = f(head) :: tail.map(f)
}