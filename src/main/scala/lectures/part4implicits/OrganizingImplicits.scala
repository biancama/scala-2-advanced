package lectures.part4implicits

object OrganizingImplicits extends App {

  implicit val reverseOrdering: Ordering[Int] = Ordering.fromLessThan(_ > _) // not considered as implicit because parentheses
  implicit def reverseOrderingWithParenthesis(): Ordering[Int] = Ordering.fromLessThan(_ > _) // not considered as implicit because parentheses
  //implicit val normalOrdering: Ordering[Int] = Ordering.fromLessThan(_ < _)
  println(List(1,4,5,3,2).sorted)  // scala look into scala.Predef

  /*
     Implicits (used as implicit parameters):
       - val/var
       - object
       - accessor methods = defs with no parentheses
    */

  // Exercise
  case class Person(name: String, age: Int)

  val persons = List(
    Person("Steve", 30),
    Person("Amy", 22),
    Person("John", 66)
  )
  //  object Person {
  //    implicit val alphabeticOrdering: Ordering[Person] = Ordering.fromLessThan((a, b) => a.name.compareTo(b.name) < 0)
  //  }
  //  implicit val ageOrdering: Ordering[Person] = Ordering.fromLessThan((a, b) => a.age < b.age)  // this has precedence
  //  println(persons.sorted)

    /*
    Implicit scope
    -normal scope = LOCAL SCOPE
    -imported scope
    -companions of all types involved in the method signature
      - List
      - Ordering
      - all the types involved = A or any supertype
    */
  // def sorted[B >: A](implicit ord: Ordering[B]): List[B]

  // best practise
  object AlphabeticNameOrdering {
    implicit val alphabeticOrdering: Ordering[Person] = Ordering.fromLessThan((a, b) => a.name.compareTo(b.name) < 0)
  }

  object AgeOrdering {
    implicit val ageOrdering: Ordering[Person] = Ordering.fromLessThan((a, b) => a.age < b.age)
  }

  import AgeOrdering._

  println(persons.sorted)

  /*
      Exercise.
      - totalPrice = most used (50%)
      - by unit count = 25%
      - by unit price = 25%
     */
  case class Purchase(nUnits: Int, unitPrice: Double)

  object Purchase {
    implicit val totalPriceOrdering : Ordering[Purchase] = Ordering.fromLessThan((p1, p2) => p1.nUnits * p1.unitPrice < p2.nUnits * p2.unitPrice)
  }

  object UnitCountPurchaseOrdering {
    implicit val unitCountOrdering : Ordering[Purchase] = Ordering.fromLessThan((p1, p2) => p1.nUnits < p2.nUnits)
  }

  object UnitPricePurchaseOrdering {
    implicit val unitCountOrdering: Ordering[Purchase] = Ordering.fromLessThan((p1, p2) => p1.unitPrice < p2.unitPrice)
  }
}
