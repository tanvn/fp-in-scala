package traits

trait Account {
  val balance: Int = 100
}

abstract class SavingsAccount extends Account with Logger {

  def withdraw(amount: Double) {

    if (amount > balance) log("Insufficient funds")
    else {
//      amount - balance
    }

  }

}

object SavingsAccount {

  def main(args: Array[String]): Unit = {
    val myAcc = new SavingsAccount with ConsoleLogger

    myAcc.withdraw(200)
    val acct1 = new SavingsAccount with TimestampLogger with ShortLogger

    val acct2 = new SavingsAccount with ShortLogger with TimestampLogger
    acct1.withdraw(120)
    acct2.withdraw(120)

    //error
//    val acct = new SavingsAccount with FileLogger {
//
//      val filename = "myapp.log" // Does not work
//
//    }

    //early definition
    val acct = new { // Early definition block after new

      val filename = "myapp.log"

    } with SavingsAccount with FileLogger
    acct.withdraw(300)

  }
}
