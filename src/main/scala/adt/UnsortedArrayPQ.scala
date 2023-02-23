package adt

import scala.reflect.ClassTag

class UnsortedArrayPQ[A : ClassTag](higherP: (A, A) => Boolean) extends PriorityQueue[A] {
  private var pq = Array.fill(10)(null.asInstanceOf[A])
  private var end = 0

  def enqueue(a: A): Unit = {
    if (end >= pq.length) {
      val tmp = Array.fill(2 * pq.length)(null.asInstanceOf[A])
      for (i <- pq.indices) tmp(i) = pq(i)
      pq = tmp
    }
    pq(end) = a
    end += 1
  }

  def dequeue(): A = {
    val highest = indexOfHighestPriority()
    val ret = pq(highest)
    end -= 1
    pq(highest) = pq(end)
    ret
  }

  def peek: A = pq(indexOfHighestPriority())

  def isEmpty: Boolean = end == 0

  def indexOfHighestPriority(): Int = {
    var highest = 0
    for (i <- 1 until end) {
      if (higherP(pq(i), pq(highest))) highest = i
    }
    highest
  }
}