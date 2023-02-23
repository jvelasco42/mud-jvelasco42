package adt

import scala.collection.mutable

class BSTMap[K, V](lt: (K, K) => Boolean) extends mutable.Map[K, V] {
  import BSTMap.Node
  private var root: Node[K, V] = null

  def get(key: K): Option[V] = {
    var rover = root
    while (rover != null && rover.kv._1 != key) {
      if (lt(key, rover.kv._1)) rover = rover.left
      else rover = rover.right
    }
    if (rover == null) None else Some(rover.kv._2)
    // Option(rover).map(_._2)
  }

  def iterator = new Iterator[(K, V)] {
    val stack = new mutable.Stack[Node[K, V]]
    pushAllLeft(root)
    def pushAllLeft(n: Node[K, V]): Unit = if (n != null) {
      stack.push(n)
      pushAllLeft(n.left)
    }
    def hasNext: Boolean = stack.nonEmpty
    def next(): (K, V) = {
      val ret = stack.pop()
      pushAllLeft(ret.right)
      ret.kv
    }
  }

  def += (kv: (K, V)) = {
    def helper(n: Node[K, V]): Node[K, V] = {
      if (n == null) {
        new Node(kv, null, null)
      } else {
        if (kv._1 == n.kv._1) {
          n.kv = kv
        } else if (lt(kv._1, n.kv._1)) {
          n.left = helper(n.left)
        } else {
          n.right = helper(n.right)
        }
        n
      }
    }
    root = helper(root)
    this
  }

  def -= (key: K) = {
    def findVictim(n: Node[K, V]): Node[K, V] = {
      if (n != null) {
        if (key == n.kv._1) {
          if (n.left == null) n.right
          else if (n.right == null) n.left
          else {
            val (newkv, newLeft) = findBiggest(n.left)
            n.kv = newkv
            n.left = newLeft
            n
          }
        } else {
          if (lt(key, n.kv._1)) n.left = findVictim(n.left)
          else n.right = findVictim(n.right)
          n
        }
      } else n
    }
    def findBiggest(n: Node[K, V]): ((K, V), Node[K, V]) = {
      if (n.right == null) {
        (n.kv, n.left)
      } else {
        val (newkv, newRight) = findBiggest(n.right)
        n.right = newRight
        (newkv, n)
      }
    }
    root = findVictim(root)
    this
  }

  def preorder(n: Node[K, V], visitor: V => Unit): Unit = {
    if (n != null) {
      visitor(n.kv._2)
      preorder(n.left, visitor)
      preorder(n.right, visitor)
    }
  }
  def postorder(n: Node[K, V], visitor: V => Unit): Unit = {
    if (n != null) {
      postorder(n.left, visitor)
      postorder(n.right, visitor)
      visitor(n.kv._2)
    }
  }
  def inorder(n: Node[K, V], visitor: V => Unit): Unit = {
    if (n != null) {
      inorder(n.left, visitor)
      visitor(n.kv._2)
      inorder(n.right, visitor)
    }
  }
}

object BSTMap {
  private class Node[K, V](var kv: (K, V), var left: Node[K, V], var right: Node[K, V])

  // def main(args: Array[String]): Unit = {
  //   val bst = new BSTMap[Int, String](_ < _)
  //   bst += 5 -> "five" += 3 -> "three" += 7 -> "seven" += 4 -> "four" 
  //   bst += 9 -> "nine" += 2 -> "two" += 6 -> "six"

  //   bst.inorder(bst.root, println)
  // }
}