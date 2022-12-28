package lectures.part2afp

object CurriesPAF extends App {

  // curried functions
  val superAdder: Int => Int => Int = x => y => x + y

  // lifting = ETA-EXPANSION
  val add3 = superAdder(3) // Int => Int = y => 3 + y
  println(add3(5))

  println(superAdder(3)(5)) // curried function

  // METHOD!
  def curriedAdder(x: Int)(y: Int): Int = x + y

  val add4: Int => Int = curriedAdder(4)
  val add5 = curriedAdder(5)
  println(add5(7))

  // method (def) cannot used as High order function because instance of an object
  // val are instances of Function[.....

  // functions != methods (JVM limitation)
  def inc(x: Int) = x + 1

  List(1, 2, 3).map(x => inc(x)) // ETA-expansion

  // Partial function applications force ETA-expansion
  val add6 = curriedAdder(6) _ // Int => Int

  // EXERCISE
  val simpleAddFunction = (x: Int, y: Int) => x + y
  def simpleAddMethod(x: Int, y: Int) = x + y
  def curriedAddMethod(x: Int)(y: Int) = x + y
  // add7: Int => Int = y => 7 + y
  // as many different implementations of add7 using the above
  // be creative!
  val add7 = y => simpleAddFunction(7, y)
  val add7_1 = y => simpleAddMethod(7, y)
  val add7_2 = curriedAddMethod(7)
  val add7_3 = curriedAddMethod(7) _

  val add7_4 = simpleAddFunction.curried(7)
  val add7_7 = simpleAddFunction(7, _: Int) // works as well

  val add7_5 = curriedAddMethod(7) _ // PAF
  val add7_6 = curriedAddMethod(7)(_) // PAF = alternative syntax

  val add7_8 = simpleAddMethod(7, _: Int) // alternative syntax for turning methods into function values


  // underscores are powerful
  def concatenator(a: String, b: String, c: String) = a + b + c

  val insertName = concatenator("Hello, I'm ", _: String, ", how are you?") // x: String => concatenator(hello, x, howarewyou)
  println(insertName("Daniel"))

  val fillInTheBlanks = concatenator("Hello, ", _: String, _: String) // (x, y) => concatenator("Hello, ", x, y)
  println(fillInTheBlanks("Daniel", " Scala is awesome!"))
  // EXERCISES
  /*
    1.  Process a list of numbers and return their string representations with different formats
        Use the %4.2f, %8.6f and %14.12f with a curried formatter function.
   */
  println("%4.2f".format(Math.PI))
  def curriedFormatter(format: String)(number: Double): String = format.format(number)

  val numbers = List(Math.PI, Math.E, 1, 9.8, 1.3e-12)
  val simpleFormat = curriedFormatter("%4.2f")
  val seriousFormat = curriedFormatter("%8.6f")
  val preciseFormat = curriedFormatter("%14.12f")

  println(numbers map preciseFormat)
    /*
      2.  difference between
          - functions vs methods
          - parameters: by-name vs 0-lambda
     */
  def byName(n: => Int) = n + 1
  def byFunction(f: () => Int) = f() + 1

  def method: Int = 42
  def parenMethod(): Int = 42
  /*
     calling byName and byFunction
     - int
     - method
     - parenMethod
     - lambda
     - PAF
    */

  byName(23) // ok
  byName(method) // ok
  byName(parenMethod())
  // byName(parenMethod) // not ok
  //  byName(() => 42) // not ok
  byName((() => 42) ()) // ok
  //  byName(parenMethod _) // not ok

  //  byFunction(45) // not ok
  //  byFunction(method) // not ok!!!!!! does not do ETA-expansion!
  byFunction(parenMethod) // compiler does ETA-expansion
  byFunction(() => 46) // works
  byFunction(parenMethod _) // also works, but warning- unnecessary


}
