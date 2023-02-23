package adt

class MutableSLL[A] extends MutableList[A] {
  import MutableSLL._
  private var head: Node[A] = null
  private var tail: Node[A] = null
  private var size = 0

  def apply(index: Int): A = {
    var rover = head
    for (i <- 0 until index) rover = rover.next
    rover.data
  }

  def insert(elem: A, index: Int): Unit = {
    if (index == 0) {
      head = new Node[A](elem, head)
      if (size == 0) tail = head
    } else {
      var rover = head
      for (i <- 0 until index-1) rover = rover.next
      rover.next = new Node[A](elem, rover.next)
      if (rover == tail) tail = rover.next
    }
    size += 1
  }

  def length: Int = size

  def remove(index: Int): A = {
    size -= 1
    if (index == 0) {
      val ret = head.data
      head = head.next
      if (head == null) tail = null
      ret
    } else {
      var rover = head
      for (i <- 0 until index-1) rover = rover.next
      val ret = rover.next.data
      rover.next = rover.next.next
      if (rover.next == null) tail = rover
      ret
    }
  }

  def update(index: Int, elem: A): Unit = {
    var rover = head
    for (i <- 0 until index) rover = rover.next
    rover.data = elem
  }

  def iterator = new Iterator[A] {
    private var rover = head
    def hasNext: Boolean = rover != null
    def next(): A = {
      val ret = rover.data
      rover = rover.next
      ret
    }
  }
}

object MutableSLL {
  private class Node[A](var data: A, var next: Node[A])
}