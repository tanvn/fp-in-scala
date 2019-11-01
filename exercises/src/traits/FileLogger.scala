package traits

import java.io.PrintStream

trait FileLogger extends Logger {

  val filename: String

  val out = new PrintStream(filename)

  def log(msg: String) { out.println(msg); out.flush() }

}
