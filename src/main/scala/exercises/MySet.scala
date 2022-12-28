package exercises

import scala.annotation.{tailrec}

trait MySet[A] extends (A => Boolean) {
  /*
      EXERCISE - implement a functional set
     */
  override def apply(a: A): Boolean = contains(a)
  def contains(elem: A): Boolean
  def +(elem: A): MySet[A]
  def ++(anotherSet: MySet[A]): MySet[A]
  def map[B](f: A => B): MySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B]
  def filter(predicate: A => Boolean): MySet[A]
  def forEach(f: A => Unit): Unit

  // EXERCISE #3 - implement a unary_! = NEGATION of a set
  // set[1,2,3] =>
  def unary_! : MySet[A]

  /*
      EXERCISE #2
      - removing an element
      - intersection with another set
      - difference with another set
     */
  def -(elem: A): MySet[A] = filter(x => x != elem)
  def --(anotherSet: MySet[A]): MySet[A] = filter(!anotherSet)

    //filter(x => !anotherSet.contains(x)) //filter(!anotherSet)
  def &(anotherSet: MySet[A]): MySet[A] = filter(anotherSet)

    /*
    filter(x => anotherSet(x))

    filter(x => anotherSet.contains(x))
    */
}
object MySet {
  /*
      val s = MySet(1,2,3) = buildSet(seq(1,2,3), [])
      = buildSet(seq(2,3), [] + 1)
      = buildSet(seq(3), [1] + 2)
      = buildSet(seq(), [1, 2] + 3)
      = [1,2,3]
     */
  def apply[A](some: A*): MySet[A] = {
    def buildSet(some:Seq[A], acc: MySet[A]) : MySet[A] = {
      some.foldLeft(acc)((set, elem) => set + elem)
    }

    buildSet(some.toSeq, new MyEmptySet[A])
  }
}
class MyEmptySet[A] extends MySet[A] {
  override def contains(elem: A): Boolean = false

  override def +(elem: A): MySet[A] = new MyNonEmptySet[A](elem, this)

  override def ++(anotherSet: MySet[A]): MySet[A] = anotherSet

  override def map[B](f: A => B): MySet[B] = new MyEmptySet[B]

  override def flatMap[B](f: A => MySet[B]): MySet[B] = new MyEmptySet[B]

  override def filter(predicate: A => Boolean): MySet[A] = this

  override def forEach(f: A => Unit): Unit = ()

  def unary_! : MySet[A] = new PropertyBasedSet[A](_ => true)

}


class AllInclusiveSet[A] extends MySet[A] {
  override def contains(elem: A): Boolean = true

  override def +(elem: A): MySet[A] = this

  override def ++(anotherSet: MySet[A]): MySet[A] = this

  // all integers => (_ % 3) => [0 1 2]
  override def map[B](f: A => B): MySet[B] = ???

  override def flatMap[B](f: A => MySet[B]): MySet[B] = ???

  override def filter(predicate: A => Boolean): MySet[A] = ???

  override def forEach(f: A => Unit): Unit = ???

  override def unary_! : MySet[A] = new MyEmptySet[A]
}

// all elements of type A which satisfy a property
class PropertyBasedSet[A](property: A => Boolean) extends MySet[A] {
  override def contains(elem: A): Boolean = property(elem)

  // { x in A | property(x) } + element = { x in A | property(x) || x == element }
  override def +(elem: A): MySet[A] = new PropertyBasedSet[A](x => property(x) || x == elem )

  override def ++(anotherSet: MySet[A]): MySet[A] = new PropertyBasedSet[A](x => property(x) || anotherSet.contains(x))

  override def map[B](f: A => B): MySet[B] = politelyFail

  override def flatMap[B](f: A => MySet[B]): MySet[B] = politelyFail

  override def filter(predicate: A => Boolean): MySet[A] = new PropertyBasedSet[A](x => property(x) && predicate(x))

  override def forEach(f: A => Unit): Unit = politelyFail

  override def unary_! : MySet[A] = new PropertyBasedSet[A](x => !property(x))

  def politelyFail = throw new IllegalArgumentException("Really deep rabbit hole!")
}

class MyNonEmptySet[A](head: A, tail: MySet[A]) extends MySet[A] {
  override def contains(elem: A): Boolean = head == elem || tail.contains(elem)

  override def +(elem: A): MySet[A] = if (this contains elem) this else new MyNonEmptySet[A](elem, this)
  /*
      [1 2 3] ++ [4 5] =
      [2 3] ++ [4 5] + 1 =
      [3] ++ [4 5] + 1 + 2 =
      [] ++ [4 5] + 1 + 2 + 3
      [4 5] + 1 + 2 + 3 = [4 5 1 2 3]
     */
  override def ++(anotherSet: MySet[A]): MySet[A] = tail ++ anotherSet + head

  override def map[B](f: A => B): MySet[B] = (tail map f) + f(head)

  override def flatMap[B](f: A => MySet[B]): MySet[B] = f(head) ++ tail.flatMap(f)

  override def filter(predicate: A => Boolean): MySet[A] = {
    val filteredTail = tail filter predicate
    if (predicate(head)) filteredTail + head
    else filteredTail
  }

  override def forEach(f: A => Unit): Unit = {
    f(head)
    tail forEach f
  }
  def unary_! : MySet[A] = new PropertyBasedSet[A](!contains(_))

}

object MySetPlayground extends App {
  import exercises.MySet
  val s = MySet[Int](1,2,3,4)
  s + 5 ++ MySet( -1, -2) + 3 map (_ + 10) flatMap (x => MySet(x, 10 * x)) filter (_ % 2 == 0)  forEach println

  val negative = !s // s.unary_! = all the naturals not equal to 1,2,3,4
  println(negative(2))
  println(negative(5))

  val negativeEven = negative.filter(_ % 2 == 0)
  println(negativeEven(5))

  val negativeEven5 = negativeEven + 5 // all the even numbers > 4 + 5
  println(negativeEven5(5))
}