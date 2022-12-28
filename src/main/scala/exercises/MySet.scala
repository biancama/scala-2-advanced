package exercises

import scala.annotation.tailrec

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

}

object MySetPlayground extends App {
  import exercises.MySet
  val s = MySet[Int](1,2,3,4)
  s + 5 ++ MySet( -1, -2) + 3 map (_ + 10) flatMap (x => MySet(x, 10 * x)) filter (_ % 2 == 0)  forEach println

}