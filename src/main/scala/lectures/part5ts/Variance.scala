package lectures.part5ts

object Variance extends App {
  trait Animal
  class Dog extends Animal
  class Cat extends Animal
  class Crocodile extends Animal
  // what is variance?
  // "inheritance" - type substitution of generics

  class Cage[T]

  // yes - covariance
  class CCage[+T]
  val ccage: CCage[Animal] = new CCage[Cat]

  // no - invariance
  class ICage[T]
  // val icage: ICage[Animal] = new ICage[Cat]
  //  val x: Int = "hello"

  // hell no - opposite = contravariance
  class XCage[-T]
  val xcage: XCage[Cat] = new XCage[Animal]

  class InvariantCage[T](val animal: T) // invariant
  // covariant positions
  class CovariantCage[+T](val animal: T) // COVARIANT POSITION

  // class ContravariantCage[-T](val animal: T) not compile because in covariant position
 /* I could use
  val catCage: XCage[Cat] = new XCage[Animal](new Crocodile)
 */

  // class CovariantVariableCage[+T](var animal: T) // types of vars are in CONTRAVARIANT POSITION   val is ok
  /*
  otherwise I could
      val ccage: CCage[Animal] = new CCage[Cat](new Cat)
      ccage.animal = new Crocodile
     */

  //class ContravariantVariableCage[-T](var animal: T) // also in COVARIANT POSITION
  /*
  otherwise I could
      val catCage: XCage[Cat] = new XCage[Animal](new Crocodile)
     */
  class InvariantVariableCage[T](var animal: T) // ok


  //  trait AnotherCovariantCage[+T] {
  //    def addAnimal(animal: T) // CONTRAVARIANT POSITION
  //  }
  /*
  otherwise I could
    val ccage: CCage[Animal] = new CCage[Dog]
    ccage.add(new Cat)
   */

  class AnotherContravariantCage[-T] {
    def addAnimal(animal: T) = true
  }

  val acc: AnotherContravariantCage[Cat] = new AnotherContravariantCage[Animal]
  acc.addAnimal(new Cat)

  class Kitty extends Cat

  acc.addAnimal(new Kitty)


  class MyList[+A] {
    def add[B >: A](element: B): MyList[B] = new MyList[B] // widening the type
  }

  val emptyList = new MyList[Kitty]
  val animals = emptyList.add(new Kitty)
  val moreAnimals = animals.add(new Cat)
  val evenMoreAnimals = moreAnimals.add(new Dog)  // become animals
  // METHOD ARGUMENTS ARE IN CONTRAVARIANT POSITION.

  // return types
  class PetShop[-T] {
    // def get(isItaPuppy: Boolean): T // METHOD RETURN TYPES ARE IN COVARIANT POSITION
    /*
          val catShop = new PetShop[Animal] {
            def get(isItaPuppy: Boolean): Animal = new Cat
          }
          val dogShop: PetShop[Dog] = catShop
          dogShop.get(true)   // EVIL CAT!
         */
    def get[S <: T](isItaPuppy: Boolean, defaultAnimal: S): S = defaultAnimal
  }

  val shop: PetShop[Dog] = new PetShop[Animal]

  // val evilCat = shop.get(true, new Cat)  cat is not a child class of dog
  class TerraNova extends Dog

  val bigFurry = shop.get(true, new TerraNova)

  /*
      Big rule
      - method arguments are in CONTRAVARIANT position
      - return types are in COVARIANT position
     */

  /**
   * 1. Invariant, covariant, contravariant
   * Parking[T](things: List[T]) {
   * park(vehicle: T)
   * impound(vehicles: List[T])
   * checkVehicles(conditions: String): List[T]
   * }
   *
   * 2. used someone else's API: IList[T]
   * 3. Parking = monad!
   *     - flatMap
   */

  class Vehicle
  class Bike extends Vehicle
  class Car extends Vehicle
  class IList[T]

  class IParking[T](things: List[T]) {
    def park[T](vehicle: T) : IParking[T] = ???
    def impound[T](vehicles: List[T]) : IParking[T] = ???
    def checkVehicles(conditions: String): List[T] = ???
    def flatMap[B](f: T => IParking[B]): IParking[B] = ???
  }

  class CParking[+T](things: List[T]) {
    def park[A >: T]  (vehicle: A) : CParking[T] = ???
    def impound[A >: T] (vehicles: List[A]): CParking[T] = ???
    def checkVehicles(conditions: String): List[T] = ???
    def flatMap[B](f: T => CParking[B]): CParking[B] = ???
  }

  val bikes:CParking[Vehicle] = CParking(List[Bike]())
  val cars:CParking[Vehicle] = CParking(List[Car]())

  class XParking[-T](things: List[T]) {
    def park(vehicle: T): XParking[T] = ???
    def impound(vehicles: List[T]): XParking[T] = ???
    def checkVehicles[S <: T](conditions: String): List[S] = ???
    def flatMap[S <: T, B](f: S => XParking[B]): XParking[B] = ???
  }
  /*
      Rule of thumb
      - use covariance = COLLECTION OF THINGS
      - use contravariance = GROUP OF ACTIONS  actions you want to perform
     */

  // with Ilist Invariant is the same
  class IListParking[T](things: IList[T]) {
    def park[T](vehicle: T): IListParking[T] = ???
    def impound[T](vehicles: IList[T]): IListParking[T] = ???
    def checkVehicles(conditions: String): IList[T] = ???
    def flatMap[B](f: T => IParking[B]): IParking[B] = ???
  }

  class CListParking[+T](things: IList[T]) {
    def park[A >: T](vehicle: A): CListParking[T] = ???
    def impound[A >: T](vehicles: IList[A]): CListParking[T]  = ???
    def checkVehicles[A >: T](conditions: String): IList[A] = ???
    def flatMap[B](f: T => CListParking[B]): CListParking[B] = ???
  }

  class XListParking[-T](things: IList[T]) {
    def park(vehicle: T): XListParking[T] = ???
    def impound[S <: T](vehicles: IList[S]): XListParking[T] = ???
    def checkVehicles[S <: T](conditions: String): IList[S] = ???
    def flatMap[S <: T, B](f: S => XListParking[B]): XListParking[B] = ???
  }
}
