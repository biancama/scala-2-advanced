package lectures.part4implicits

import javax.print.attribute.standard.JobHoldUntil

object TypeClasses extends App {
  trait HTMLWritable {
    def toHtml: String
  }

  case class User(name: String, age: Int, email: String) extends HTMLWritable {
    override def toHtml: String = s"<div>$name ($age yo) <a href=$email/> </div>"
  }

  User("John", 32, "john@rockthejvm.com").toHtml // works with disadvantages
  /*
      1 - for the types WE write
      2 - ONE implementation out of quite a number
     */

  // option 2 - pattern matching
  object HTMLSerializerPM {
    def serializeToHtml(value: Any) = value match {
      case User(n, a, e) =>
      // case java.util.Date =>
      case _ =>
    }
  }

  /*
      1 - lost type safety
      2 - need to  modify the code every time
      3 - still ONE implementation (i.e if the user is logged in we should show different info if the user is not logged in
     */

  // better implementation
  trait HTMLSerializer[T] {
    def serialize(value: T): String
  }

  implicit object UserSerializer extends HTMLSerializer[User] {
    def serialize(user: User): String = s"<div>${user.name} (${user.age} yo) <a href=${user.email}/> </div>"
  }

  val john = User("John", 32, "john@rockthejvm.com")
  println(UserSerializer.serialize(john))

  // 1 - we can define serializers for other  types

  import java.util.Date

  object DateSerializer extends HTMLSerializer[Date] {
    override def serialize(date: Date): String = s"<div>${date.toString()}</div>"
  }

  // 2 - we can define MULTIPLE serializers
  object PartialUserSerializer extends HTMLSerializer[User] {
    def serialize(user: User): String = s"<div>${user.name} </div>"
  }
  // TYPE CLASS
  trait MyTypeClassTemplate[T] {
    def action(value: T): String
  }

  object MyTypeClassTemplate {
    def apply[T](implicit instance: MyTypeClassTemplate[T]) = instance
  }

  /**
   * Equality
   */
  trait Equal[T] {
    def equal(o1: T, o2: T): Boolean
  }

  implicit object NameEquality extends Equal[User] {
    override def equal(o1: User, o2: User): Boolean = o1.name == o2.name
  }
  object FullyEquality extends Equal[User] {
    override def equal(o1: User, o2: User): Boolean = o1.name == o2.name && o1.email == o2.email
  }

  // part 2
  object HTMLSerializer {
    def serialize[T](value: T)(implicit serializer: HTMLSerializer[T]): String =
      serializer.serialize(value)

    def apply[T](implicit serializer: HTMLSerializer[T]) = serializer
  }
  implicit object IntSerializer extends HTMLSerializer[Int] {
    override def serialize(value: Int): String = s"<div style: color=blue>$value</div>"
  }

  println(HTMLSerializer.serialize(42))
  println(HTMLSerializer.serialize(john))

  // access to the entire type class  interface
  println(HTMLSerializer[User].serialize(john))


  /**
   * Exercise: Implement the TC for the equality
   *
   */
  object Equal {
    def apply[T](implicit instance: Equal[T]): Equal[T] = instance
  }

  val anotherJohn = User("John", 45, "anotherjohn@rtjvm.com")
  println(Equal[User].equal(john, anotherJohn))
    // Ad-hoc polymorphism
}
