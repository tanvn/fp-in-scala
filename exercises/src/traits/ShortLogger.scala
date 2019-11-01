package traits

trait ShortLogger extends ConsoleLogger {

  override def log(msg: String) {

    super.log(if (msg.length <= 15) msg else s"${msg.substring(0, 12)}...")

  }

}
