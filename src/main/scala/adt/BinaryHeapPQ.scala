package adt

import scala.reflect.ClassTag

class BinaryHeapPQ[A : ClassTag](higherP: (A, A) => Boolean) extends PriorityQueue[A] {
  private var heap = Array.fill(10)(null.asInstanceOf[A])
  private var end = 1

  def dequeue(): A = {
    val ret = heap(1)
    end -= 1
    val tmp = heap(end)
    var flag = true
    var stone = 1
    while (flag && stone * 2 < end) {
      var higherChild = stone * 2
      if (stone * 2 + 1 < end && higherP(heap(stone * 2 + 1), heap(stone * 2))) higherChild += 1 
      if (higherP(heap(higherChild), tmp)) {
        heap(stone) = heap(higherChild)
        stone = higherChild
      } else flag = false
    }
    heap(stone) = tmp
    ret
  }

  def enqueue(a: A): Unit = {
    if (end >= heap.length) {
      val tmp = Array.fill(heap.length * 2)(null.asInstanceOf[A])
      for (i <- 1 until heap.length) tmp(i) = heap(i)
      heap = tmp
    }
    var bubble = end
    while (bubble > 1 && higherP(a, heap(bubble/2))) {
      heap(bubble) = heap(bubble / 2)
      bubble /= 2
    }
    heap(bubble) = a
    end += 1
  }

  def isEmpty: Boolean = end == 1

  def peek: A = heap(1)

}