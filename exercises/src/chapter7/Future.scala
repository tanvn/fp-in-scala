package chapter7

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService, Executors}
import java.util.concurrent.atomic.AtomicReference

sealed trait Future[A] {
  private[chapter7] def apply(k: A => Unit): Unit

}

object Future {
  type Par[A] = ExecutorService => Future[A]

  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    val cb = { a: A =>
      println(s"run, got a=$a")
      ref.set(a); latch.countDown();
    }
    println(s"in run, p=$p, cb=$cb")

    p(es)(cb)
    latch.await()
    ref.get()
  }

  def unit[A](a: A): Par[A] =
    _ =>
      new Future[A] {
        override def apply(cb: A => Unit) = {
          println("unit called")
          cb(a)
        }
      }

  def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] {
      override def call(): Unit = r
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  //here, when the main body is executed, value of a: => Par[A] is equal to parMap body
  def fork[A](a: => Par[A]): Par[A] =
    es => {
      // equal to p(es) of line 23, when line 23: p(es)(cb) is executed, it is equal to the below Future's apply is called
      new Future[A] {
        override private[chapter7] def apply(cb: A => Unit): Unit = {
          println(s"${Thread.currentThread().getName} in fork $cb")
          // a is parMap body, run a(es) in a new thread
          eval(es)(a(es)(cb))
        }

      }
    }

  // return a function (es) => Future
  // line 52 : eval(es)(a(es)(cb)), a(es) will return the future Future[C] below and execute the apply method
  // p : Par[A] is unit(List.empty[A]) and p2 :Par[B] = lazyUnit(math.sqrt(1))
  // and the function f is (l, f) => l :+ f (append f to the List l)
  def map2[A, B, C](p: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] = { es =>
    new Future[C] {
      def apply(cb: C => Unit) = {
        println(s"${Thread.currentThread().getName} in map2, cb=$cb")
        var ar: Option[A] = None
        var br: Option[B] = None
        val combiner = Actor[Either[A, B]](es) {
          case Left(a) =>
            br match {
              case None    => ar = Some(a) // List()
              case Some(b) => eval(es)(cb(f(a, b)))
            }
          case Right(b) =>
            ar match {
              case None => br = Some(b)
              case Some(a) =>
                eval(es)(cb(f(a, b))) // f(a, b) = List() :+ 1 => List(1), => cb(List(1))
            }
        }
        // p : Par[A] is unit(List.empty[A])
        p(es)(a => {
          println(s"${Thread.currentThread().getName} send a=$a")
          combiner ! Left(a)
        })
        //p2 :Par[B] = lazyUnit(math.sqrt(1))
        p2(es)(b => {
          println(s"${Thread.currentThread().getName} send b=$b")
          combiner ! Right(b)
        })
      }
    }
  }

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def asyncF[A, B](f: A => B): A => Par[B] = a => {
    println(s"${Thread.currentThread().getName} asyncF for a=$a")
    lazyUnit(f(a))
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    println(s"${Thread.currentThread().getName} sequence for List=$ps")
    ps.foldLeft(unit(List.empty[A]))((l, f) => {
      println(s"in foldLeft $l, $f")
      map2(l, f)((l, f) => l :+ f)
    })
  }

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    println(s"${Thread.currentThread().getName} parMap for List=$ps")
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    val res = sequence(fbs)
    println(s"${Thread.currentThread().getName} res=$res")
    // here with ps is List(1), res will be the result of map2(unit(List.empty[A]), lazyUnit(math.sqrt(1)))((l, f) => l :+ f))
    // the foldLeft is called once
    res
  }

  def main(args: Array[String]): Unit = {
    val p: Par[List[Double]] = parMap(List.range(1, 3))(math.sqrt(_))
    println(s"${Thread.currentThread().getName} run $p")
    val x: Seq[Double] = run(Executors.newFixedThreadPool(2))(p)
    println(s"got $x")
  }
}
