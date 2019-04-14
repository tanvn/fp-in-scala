package chapter4

case class Person(name: Name, age: Age)
sealed case class Name( value: String)
sealed case class Age( value: Int)

object Person {
  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.")
    else Right(new Age(age))

  def mkPerson(name: String, age: Int): Either[String, Person] =
    mkName(name).map2(mkAge(age))(Person(_, _))

  def main(args: Array[String]): Unit = {
    println(mkPerson("David", 24))

    println(mkPerson("", -2))
  }
}

