package lectures.part4implicits

import java.util.Date

object JSONSerialization extends App {
  /*
      Users, posts, feeds
      Serialize to JSON
     */

  case class User(name: String, age: Int, email: String)
  case class Post(content: String, createdAt: Date)
  case class Feed(user: User, posts: List[Post])

  /*
      1 - intermediate data types: Int, String, List, Date
      2 - type classes for conversion to intermediate  data types
      3 - serialize to JSON
     */

  sealed trait JSONValue { // intermediate data type
    def stringify: String
  }


  // type class

  final case class JSONString(value: String) extends JSONValue {
    override def stringify: String = "\"" + value +"\""
  }

  final case class JSONNumber(value: Int) extends JSONValue {
    override def stringify: String = value.toString
  }

  final case class JSONArray(value: List[JSONValue]) extends JSONValue {
    override def stringify: String = value.map(obj => obj.stringify).mkString("[", ",", "]")
  }
  final case class JSONObject(values: Map[String, JSONValue]) extends JSONValue {
    /*
         {
           name: "John"
           age: 22
           friends: [ ... ]
           latestPost: {
             content: "Scala Rocks"
             date: ...
           }
         }
        */
    override def stringify: String = values.map{case (name, obj) => "\"" + name + "\": " + obj.stringify}
      .mkString("{", ",", "}")
  }

  val data = JSONObject(Map(
    "user" -> JSONString("Daniel"),
    "posts" -> JSONArray(List(
      JSONString("Scala Rocks!"),
      JSONNumber(453)
    ))
  ))

  println(data.stringify)

  // type class
  /*
    1 - type class
    2 - type class instances (implicit)
    3 - pimp library to use type class instances
   */

  // 2.1
  trait JSONConverter[T] {
    def convert(value: T): JSONValue
  }

  // 2.3  conversion
   implicit class JSONOps[T](value: T) {
     def toJSON(implicit converter: JSONConverter[T]): JSONValue = converter.convert(value)
   }


  implicit object StringConverter extends JSONConverter[String] {
    override def convert(value: String): JSONValue = JSONString(value)
  }

  implicit object NumberConverter extends JSONConverter[Int] {
    override def convert(value: Int): JSONValue = JSONNumber(value)
  }

  implicit object UserConverter extends JSONConverter[User] {
    override def convert(user: User): JSONValue = JSONObject(Map(
      //"name" -> StringConverter.convert(user.name),
      "name" -> user.name.toJSON,
      //"age" -> NumberConverter.convert(user.age),
      "age" -> user.age.toJSON,
      //"email" -> StringConverter.convert(user.email)
      "email" -> user.email.toJSON
    ))
  }

  implicit object PostConverter extends JSONConverter[Post] {
    override def convert(post: Post): JSONValue = JSONObject (Map(
      //"content" -> StringConverter.convert(post.content),
      "content" -> post.content.toJSON,
      // "createdAt" -> StringConverter.convert(post.createdAt.toString)
    "createdAt" -> post.createdAt.toString.toJSON
    ))
  }

  implicit object FeedConverter extends JSONConverter[Feed] {
    override def convert(feed: Feed): JSONValue = JSONObject(Map(
      //"user" -> UserConverter.convert(feed.user),
      "user" -> feed.user.toJSON,
      //"posts" -> JSONArray(feed.posts.map(p => PostConverter.convert(p)))
      "posts" -> JSONArray(feed.posts.map(_.toJSON))
    ))
  }


  // call stringify on result
  val now = new Date(System.currentTimeMillis())
  val john = User("John", 34, "john@rockthejvm.com")
  val feed = Feed(john, List(
    Post("hello", now),
    Post("look at this cute puppy", now)
  ))

  println(FeedConverter.convert(feed).stringify)
    // thanks to implicit class JSONOps[T](value: T) {....
  println(feed.toJSON.stringify)
}
