package exercises

import lectures.part4implicits.TypeClasses.User

import scala.annotation.targetName


object EqualityPlayground extends App {
  /**
   * Equality
   */
  trait Equal[T] {
    def apply(a: T, b: T): Boolean
  }

  implicit object NameEquality extends Equal[User] {
    override def apply(a: User, b: User): Boolean = a.name == b.name
  }

  object FullEquality extends Equal[User] {
    override def apply(a: User, b: User): Boolean = a.name == b.name && a.email == b.email
  }

  /*
  Exercise: implement the TC pattern for the Equality tc.
  */
  object Equal {
    def apply[T](a: T, b: T)(implicit equalizer: Equal[T]): Boolean =
      equalizer.apply(a, b)
  }

  val john = User("John", 32, "john@rockthejvm.com")
  val anotherJohn = User("John", 45, "anotherJohn@rtjvm.com")
  println(Equal(john, anotherJohn))
  // AD-HOC polymorphism
  /*
      Exercise - improve the Equal TC with an implicit conversion class
      ===(anotherValue: T)
      !==(anotherValue: T)
     */
  implicit class EqualEnrichment[T](value: T) {
    @targetName("===")
    def ===(anotherValue: T)(implicit equalizer: Equal[T]): Boolean = equalizer.apply(value, anotherValue)
    @targetName("!==")
    def !==(anotherValue: T)(implicit equalizer: Equal[T]): Boolean = !(value === anotherValue)
  }

  println(john === anotherJohn)
  println(john !== anotherJohn)

  /*
      john.===(anotherJohn)
      new TypeSafeEqual[User](john).===(anotherJohn)
      new TypeSafeEqual[User](john).===(anotherJohn)(NameEquality)
     */
  /*
    TYPE SAFE
   */
  // println(john == 43) this won't compile
}
