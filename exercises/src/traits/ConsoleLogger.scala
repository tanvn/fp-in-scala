package traits

trait ConsoleLogger extends Logger with Cloneable with Serializable { // Use extends, not implements

  def log(msg: String) { println(msg) } // No override needed

}
