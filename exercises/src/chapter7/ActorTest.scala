package chapter7

import java.util.concurrent.Executors

object ActorTest {

  def main(args: Array[String]): Unit = {

    val handler = (msg: String) => {
      println(s"got message : $msg")
      msg

    }
    // only when call s.shutdown() then the program will terminate
    val s = Executors.newFixedThreadPool(4)
    val echoer = Actor[String](s)((msg: String) => {
      println(s"${Thread.currentThread().getName} got message : $msg")
    })
    echoer ! "hello"
    echoer ! "world"
    echoer ! "this is me"
    echoer ! "TanVn"

//    Thread.getAllStackTraces.keySet.forEach(
//      (t: Thread) =>
//        System.out.println(t.getName + " Is Daemon " + t.isDaemon + " Is Alive " + t.isAlive)
//    )
  }
}
