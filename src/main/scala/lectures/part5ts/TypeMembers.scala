package lectures.part5ts

object TypeMembers extends App {
  class Animal
  class Dog extends Animal
  class Cat extends Animal

  class AnimalCollection {
    type AnimalType // abstract type member
    type BoundedAnimal <: Animal
    type SuperBoundedAnimal >: Dog <: Animal
    type AnimalC = Cat
  }

  val ac = new AnimalCollection
  val dog: ac.AnimalType = ???
  //val cat: ac.BoundedAnimal = new Cat
  val pup: ac.SuperBoundedAnimal = new Dog

  val cat: ac.AnimalC = new Cat

  type CatAlias = Cat
  val anotherCat: CatAlias = new Cat

  // alternative to generics
  trait MyList {
    type T

    def add(element: T): MyList
  }

  class NonEmptyList(value: Int) extends MyList {
    override type T = Int
    def add(element: Int): MyList = ???
  }

  // .type
  type CatsType = cat.type
  val newCat: CatsType = cat
  //  new CatsType  this is not working

  /*
      Exercise - enforce a type to be applicable to SOME TYPES only
     */

  // LOCKED someone else wrote  this code
  trait MList {
    type A
    def head: A
    def tail: MList
  }

  // it should NOT compile
  class CustomList(hd: String, tl: CustomList ) extends MList {
    override type A = String
    override def head: A = hd
    override def tail: MList = tl
  }
  // it should  compile
  class IntList(hd: Int, tl: IntList) extends MList  {
    override type A = Int
    override def head: A = hd
    override def tail: MList = tl
  }

  // hint Number
  // type members and type member constraint (bounds)

  trait ApplicableToNumber {
    type A <: Number
  }

  /*
  class CustomListEx(hd: String, tl: CustomListEx) extends MList with ApplicableToNumber {
    override type A = String
    override def head: A = hd
    override def tail: MList = tl
  }
  */

  // it should  compile
  class IntListEx(hd: Int, tl: IntListEx) extends MList with ApplicableToNumber {
    override type A = Number

    override def head: A = hd

    override def tail: MList = tl
  }
}
