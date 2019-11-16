package chapter10

import chapter3.{Branch, Leaf, Tree}

trait Foldable[F[_]] {
  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B
  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B
  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)
  def toList[A](fa: F[A]): List[A] = foldLeft(fa)(List.empty[A])((list, b) => list :+ b)
}

class FList extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B = as.foldRight(z)(f)

  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B = {
    if (as.isEmpty) {
      mb.zero
    } else if (as.size == 1) {
      mb.op(mb.zero, f(as.head))
    } else {
      as.foldLeft(mb.zero)((b, a) => mb.op(b, f(a)))
    }
  }
}

object FStream extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B =
    as.foldRight(z)(f)

  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B = as.foldLeft(z)(f)

  override def foldMap[A, B](as: Stream[A])(f: A => B)(mb: Monoid[B]): B = {
    if (as.isEmpty) {
      mb.zero
    } else if (as.size == 1) {
      mb.op(mb.zero, f(as.head))
    } else {
      as.foldLeft(mb.zero)((b, a) => mb.op(b, f(a)))
    }
  }
}

object FOption extends Foldable[Option] {
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B = as match {
    case Some(value) => f(value, z)
    case None        => z
  }

  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
    foldRight(as)(z)((a, b) => f(b, a))

  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))
}

object FTree extends Foldable[Tree] {
  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = {
    as match {
      case Leaf(value) => f(value, z)
      case Branch(left, right) => {
        val r = foldRight(right)(z)(f)
        foldRight(left)(r)(f)
      }
    }
  }

  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = as match {
    case Leaf(value) => f(z, value)
    case Branch(left, right) => {
      val l = foldLeft(left)(z)(f)
      foldLeft(right)(l)(f)
    }
  }

  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = {
    as match {
      case Leaf(value) => mb.op(mb.zero, f(value))
      case Branch(left, right) => {
        val l = foldLeft(left)(mb.zero)((b, a) => mb.op(b, f(a)))
        val r = foldRight(right)(mb.zero)((a, b) => mb.op(b, f(a)))
        mb.op(l, r)
      }
    }
  }
}

object Foldable {

  def main(args: Array[String]): Unit = {
    val leaf1 = Leaf[Int](15)
    val leaf2 = Leaf[Int](20)
    val leaf3 = Leaf[Int](13)

    val leaf4 = Leaf[Int](9)
    val leaf5 = Leaf[Int](32)
    val leaf6 = Leaf[Int](28)
    val leaf7 = Leaf[Int](60)
    val leaf8 = Leaf[Int](51)

    val branch4 = Branch[Int](leaf8, leaf6)

    val branch3 = Branch[Int](leaf5, branch4)

    val branch1 = Branch[Int](branch3, leaf7)
    val branch2 = Branch[Int](leaf3, leaf4)

//                   root
//             b1      			     b2
//         b3     l7=60			l3=13  l4=9
//   l5=32     b4
//          l8=51 l6=28

    val root = Branch[Int](branch1, branch2)
    val flVal = FTree.foldLeft(root)(0)(_ + _)
    println(flVal)
    val frVal = FTree.foldRight(root)(0)(_ + _)
    println(frVal)

    val fMapConcat = FTree.foldMap(root)(a => a.toString)(Monoid.stringConcatMonoid)
    println(fMapConcat)

    val fMapIntPlus = FTree.foldMap(root)(identity)(Monoid.plusIntMonoid)
    println(fMapIntPlus)

    val intOpt = Some(19)
    val fROpt = FOption.foldRight(intOpt)(20)(_ + _)
    println(fROpt)
    val fLOpt = FOption.foldLeft(intOpt)(20)(_ + _)
    println(fLOpt)

    val fMOpt = FOption.foldMap(intOpt)(identity)(Monoid.plusIntMonoid)
    println(fMOpt)
    val intNone: Option[Int] = None
    val fMNone = FOption.foldMap(intNone)(identity)(Monoid.plusIntMonoid)
    println(fMNone)

    val treeToList = FTree.toList(root)
    println(treeToList)

    val stream = Stream(1, 3, 4, 5, 6)
    println(FStream.toList(stream))

  }
}
