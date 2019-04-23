package chapter3.queue

trait Queue[T] {

  def isEmpty: Boolean
  def enQueue(t: T): Queue[T]
  //Removes the element at the beginning of the immutable queue, and returns the new queue.
  def deQueue(): Queue[T]
  def head: Option[T]
}

private case class Node[T](value: Option[T], next: Option[Node[T]])
    extends Queue[T] {

  def empty[T]: Node[T] = Node(None, None)
  override def isEmpty: Boolean = value.isEmpty

  override def enQueue(t: T): Node[T] =
    if (isEmpty) Node(Some(t), None) else Node(Some(t), Some(this))

  override def head: Option[T] = value
  override def deQueue(): Node[T] = next.getOrElse(empty)

}

object Queue {
  // empty method
  def empty[T]: Queue[T] = Node(None, None)

}
