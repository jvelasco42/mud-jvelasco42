package adt

trait MutableList[A] {
  def apply(index: Int): A
  def insert(elem: A, index: Int): Unit
  def length: Int
  def remove(index: Int): A
  def update(index: Int, elem: A): Unit
  def iterator: Iterator[A]
}