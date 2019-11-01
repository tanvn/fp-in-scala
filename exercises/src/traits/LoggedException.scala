package traits

trait LoggedException extends ConsoleLogger {

  this: Exception =>

  def log() { log(getMessage()) }

}
