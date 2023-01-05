package lectures.part4implicits

object ImplicitsIntro extends App {
  val pair = "Massimo" -> "555"
  val intPair = 1 -> 2

  case class Person(name: String) {
    def greet = s"Hi, my name is $name!"
  }

  implicit def fromStringToPerson(str: String): Person = Person(str)

  println("Peter".greet)

  //  class A {
  //    def greet: Int = 2
  //  }
  //  implicit def fromStringToA(str: String): A = new A   the compiler is confused

  def increment(x: Int)(implicit amount: Int) = x + amount
  implicit val defaultAmount:Int = 10
  increment(2)
  // NOT default args
}
