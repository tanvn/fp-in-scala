package traits

trait TimestampLogger extends ConsoleLogger {

  abstract override def log(msg: String) {
//    super.log(s"${java.time.Instant.now()} $msg")
    super.log(s"${java.time.Instant.now()} $msg")

  }

}
