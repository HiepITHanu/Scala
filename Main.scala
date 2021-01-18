import scala.annotation.tailrec

object Main {
  def main(args: Array[String]): Unit ={
    val l = new ValidLinkedList(1, new ValidLinkedList(2, new ValidLinkedList(3, new InvalidLinkedList())))
    l.map(_ + 2).foreach(x=> println(x))
    println(l.size)
    println(l.apply(0))
    //    val l = new ValidItemLinkedList(1, new ValidItemLinkedList(2, new ValidItemLinkedList(3, new InvalidItemLinkedList)))
  }
}

abstract class AbstractLinkedList[T](data: T*){
  lazy val head:T = headOption.get

  def headOption: Option[T] = apply(0)

  def tail: AbstractLinkedList[T]

  lazy val size: Int = {
    @tailrec
    def tailRecSize(remaining: AbstractLinkedList[T], count: Int): Int = {
      remaining.headOption match {
        case Some(i) => tailRecSize(remaining.tail, count + 1)
        case None => count
      }
    }

    tailRecSize(this, 0)
  }

  def foreach(x: T => Unit): Unit

  def apply(index: Int): Option[T]

  def filter(x: T => Boolean): AbstractLinkedList[T]

  def map[S](x: T => S): AbstractLinkedList[S]

  lazy val reverse: AbstractLinkedList[T] = {
    @tailrec
    def tailRecReverse(remaining: AbstractLinkedList[T], temp: AbstractLinkedList[T]): AbstractLinkedList[T] = {
      remaining.headOption match {
        case Some(i) => tailRecReverse(remaining.tail, i :: temp)
        case None => temp
      }
    }

    tailRecReverse(this, new InvalidLinkedList[T])
  }

  def ::(i: T): AbstractLinkedList[T] = new ValidLinkedList[T](i, this)
}


class ValidLinkedList[T](val data: T, val next: AbstractLinkedList[T]) extends AbstractLinkedList [T]{
  override def tail: AbstractLinkedList[T] = next

  override def foreach(x: T => Unit): Unit = {
    x(data)
    tail.foreach(x)
  }

  override def apply(index: Int): Option[T] = {
    if (index < 1) Some(data)
    else tail.apply(index - 1)
  }

  override def filter(x: T => Boolean): AbstractLinkedList[T] = {
    @tailrec
    def tailRecFilter(x: T => Boolean, remaining: AbstractLinkedList[T], temp: AbstractLinkedList[T]): AbstractLinkedList[T] ={
      remaining.headOption match {
        case Some(i) if x(i) => tailRecFilter(x, remaining.tail, i :: temp)
        case Some(i) => tailRecFilter(x, remaining.tail, temp)
        case None => temp
      }
    }
    val result: AbstractLinkedList[T] = tailRecFilter(x, this, new InvalidLinkedList[T])
    result.reverse
  }

  override def map[S](x: T => S): AbstractLinkedList[S] = {
    @tailrec
    def tailRecMap[S](x: T => S, remaining: AbstractLinkedList[T], temp: AbstractLinkedList[S]): AbstractLinkedList[S] = {
      remaining.headOption match {
        case Some(i) => tailRecMap(x, remaining.tail, x(i) :: temp)
        case None => temp
      }
    }

    val result: AbstractLinkedList[S] = tailRecMap(x, this, new InvalidLinkedList[S])
    result.reverse
  }
}

class InvalidLinkedList[T] extends AbstractLinkedList [T]{
  override def tail: AbstractLinkedList[T] = null

  override def foreach(x: T => Unit): Unit = {}

  override def apply(index: Int): Option[T] = None

  override def filter(x: T => Boolean): AbstractLinkedList[T] = null

  override def map[S](x: T => S): AbstractLinkedList[S] = null
}
