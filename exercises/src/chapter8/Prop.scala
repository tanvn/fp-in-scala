package chapter8

trait Prop {

  def &&(p: Prop): Prop = new Prop {
    def check: Boolean = this.check && p.check
  }
  def check: Boolean
}
