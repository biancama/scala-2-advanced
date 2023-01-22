package lectures.part5ts

import lectures.part5ts.PathDependentTypes.ItemLike

import java.security.Key

object PathDependentTypes extends App {

  class Outer {
    class Inner
    object InnerObject
    type InnerType

    def print(i: Inner) = println(i)
    def printGeneral(i: Outer#Inner) = println(i)
  }

  def aMethod: Int = {
    class HelperClass
    type HelperType = String  /// only as alias
    2
  }

  // per-instance
  val o = new Outer
  val inner = new o.Inner // o.Inner is a TYPE  not possible Outer.Inner

  val oo = new Outer
  //val otherInner: oo.Inner = new o.Inner  // those are different types

  o.print(inner)
  //  oo.print(inner)

  // path-dependent types

  // Outer#Inner  common super type
  o.printGeneral(inner)
  oo.printGeneral(inner)

  /*
     Exercise
     DB keyed by Int or String, but maybe others
    */
  /*
  trait Item[K]
  trait IntItem extends Item[Int]
  trait StringItem extends Item[String]
*/
  /*
  def get[ItemType] (key: Key): ItemType
  get[IntItem](42)
  get[String]("home")
  get[IntItem](42) // ok
    get[StringItem]("home") // ok

    // get[IntItem]("scala") // not ok
  }
  */

  /*
      use path-dependent types
      abstract type members and/or type aliases
     */

  trait ItemLike {
    type Key
  }

  trait Item[K] extends ItemLike {
    type Key = K
  }

  trait IntItem extends Item[Int]

  trait StringItem extends Item[String]

  def get[ItemType <: ItemLike](key: ItemLike#Key): ItemType = ???


  /*
  get[IntItem](42) // ok
  get[StringItem]("home") // ok

  // get[IntItemEx]("scala") // not ok
   Not working
  */
}
