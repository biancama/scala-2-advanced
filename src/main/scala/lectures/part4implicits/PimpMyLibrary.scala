package lectures.part4implicits

import scala.annotation.{tailrec, targetName}

object PimpMyLibrary extends App {
  // 2 .isPrime

  implicit class RichInt(val value: Int) extends AnyVal {  // must take one and only argument
    def isEven: Boolean = value % 2 == 0

    def sqrt: Double = Math.sqrt(value)

    def times(fun: () => Unit): Unit = for (_ <- 1 to value) fun()

   // @targetName("*")
    def *[T](l: List[T]): List[T] = {
      @tailrec
      def app(value: Int, acc: List[T]): List[T] = {
        if (value <= 1) acc
        else app(value - 1 , l ++ acc)
      }
      app(value, l)
    }
  }

  implicit class RicherInt(richInt: RichInt) {
    def isOdd: Boolean = richInt.value % 2 != 0
  }

  new RichInt(42).sqrt

  42.isEven // new RichInt(42).isEven
  // type enrichment = pimping

  1 to 10

  import scala.concurrent.duration._

  3.seconds
  // compiler doesn't do multiple implicit searches.
  // 42.isOdd


  /*
      Enrich the String class
      - asInt
      - encrypt
        "John" -> Lqjp
      Keep enriching the Int class
      - times(function)
        3.times(() => ...)
      - *
        3 * List(1,2) => List(1,2,1,2,1,2)
     */

  implicit class RichString(value: String) {
    def asInt: Int = value.toInt
    def encrypt(cypherDistance: Int): String = value.map(c =>(c.toInt + cypherDistance).toChar)
  }

  println("4".asInt + 3)

  println("John".encrypt(2))

  3.times(() => println("times"))

  val listTimes = 3 * List(1,2)
  println(s"ListTimes: ${listTimes}")


  // "3" / 4  // implicit conversion
  implicit def stringToInt(string: String): Int = Integer.valueOf(string)

  println("6" / 2) // stringToInt("6") / 2


  // equivalent: implicit class RichAltInt(value: Int)
  class RichAltInt(value: Int)

  implicit def enrich(value: Int): RichAltInt = new RichAltInt(value)

  /// those there are discouraged...super hard to trace back if there is a bug


  // danger zone
  implicit def intToBoolean(i: Int): Boolean = i == 1
  /*
      if (n) do something
      else do something else
     */
  val aConditionedValue = if (3) "OK" else "Something wrong"
  println(aConditionedValue)
}
