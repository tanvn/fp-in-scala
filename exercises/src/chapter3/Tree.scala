package chapter3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def count[A](t: Tree[A]): Int = t match {
    case _: Leaf[A]   => 1
    case b: Branch[A] => count(b.left) + count(b.right) + 1
  }

  def maximum(t: Tree[Int]): Int = t match {
    case l: Leaf[Int]   => l.value
    case b: Branch[Int] => maximum(b.left) max maximum(b.right)

  }

  def depth[A](t: Tree[A]): Int = t match {
    case _: Leaf[A]   => 0
    case b: Branch[A] => (depth(b.left) + 1) max (depth(b.right) + 1)
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case l: Leaf[A]   => Leaf(f(l.value))
    case b: Branch[A] => Branch(left = map(b.left)(f), right = map(b.right)(f))
  }

  def main(args: Array[String]): Unit = {

    val leaf1 = Leaf[Int](15)
    val leaf2 = Leaf[Int](20)
    val leaf3 = Leaf[Int](13)

    val leaf4 = Leaf[Int](9)
    val leaf5 = Leaf[Int](32)
    val leaf6 = Leaf[Int](28)
    val leaf7 = Leaf[Int](60)
    val leaf8 = Leaf[Int](51)

    val branch4 = Branch[Int](leaf3, leaf4)

    val branch3 = Branch[Int](leaf5, branch4)

    val branch1 = Branch[Int](branch3, leaf7)
    val branch2 = Branch[Int](leaf3, leaf4)

    val root = Branch[Int](branch1, branch2)

    val stringTree = map(root)(a => a + 2)

    println(stringTree)

    println(count(root))
    println(maximum(root))
    // print root depth
    println(depth(root))

  }
}
