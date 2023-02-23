package adt

class MutableDLL[A] extends MutableList[A] {
  import MutableDLL._
  private var end: Node[A] = new Node[A](null, null.asInstanceOf[A], null)
  end.next = end
  end.prev = end
  private var size = 0

  def apply(index: Int): A = {
    var rover = end.next
    for (i <- 0 until index) rover = rover.next
    rover.data
  }

  def insert(elem: A, index: Int): Unit = {
    var rover = end.next
    for (i <- 0 until index) rover = rover.next
    val n = new Node[A](rover.prev, elem, rover)
    rover.prev.next = n
    rover.prev = n
    size += 1
  }

  def length: Int = size

  def remove(index: Int): A = {
    size -= 1
    var rover = end.next
    for (i <- 0 until index) rover = rover.next
    val ret = rover.data
    rover.next.prev = rover.prev
    rover.prev.next = rover.next
    ret
  }

  def update(index: Int, elem: A): Unit = {
    var rover = end.next
    for (i <- 0 until index) rover = rover.next
    rover.data = elem
  }

  def iterator = new Iterator[A] {
    private var rover = end.next
    def hasNext: Boolean = rover != null
    def next(): A = {
      val ret = rover.data
      rover = rover.next
      ret
    }
  }
}

object MutableDLL {
  private class Node[A](var prev: Node[A], var data: A, var next: Node[A])
}