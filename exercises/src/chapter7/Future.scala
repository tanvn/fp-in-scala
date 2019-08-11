package chapter7

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}
import java.util.concurrent.atomic.AtomicReference

sealed trait Future[A] {
  private[chapter7] def apply(k: A => Unit): Unit

}

object Future {
  type Par[A] = ExecutorService => Future[A]

  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    p(es) { a =>
      ref.set(a); latch.countDown();
    }
    latch.await()
    ref.get()
  }

  def unit[A](a: A): Par[A] = es => new Future[A] { override def apply(cb: A => Unit) = cb(a) }

  def eval(es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] {
      override def call(): Unit = r
    })

  def fork[A](a: => Par[A]): Par[A] =
    es =>
      new Future[A] {
        override private[chapter7] def apply(cb: A => Unit): Unit = eval(es)(a(es)(cb))
      }

}
