package answer.fpinscala.first.laziness

import answer.fpinscala.first.laziness.Stream.{cons, empty}

import scala.annotation.tailrec

trait Stream[+A] {
  import Stream._
  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toListRecursive: List[A] = this match {
    case Cons(h, t) => h() :: t().toListRecursive
    case Empty => Nil
  }



  def toList: List[A] = {
    @tailrec
    def toListTailrec(stream: Stream[A], acc: List[A]): List[A] = stream match {
      case Empty => acc.reverse
      case Cons(h, t) => toListTailrec(t(), h()::acc)
    }
    toListTailrec(this, Nil)
  }


  def toListFast: List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    @tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Cons(h, t) =>
        buf.addOne(h())
        go(t())
      case Empty => buf.toList
    }
    go(this)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }


  def drop(n: Int): Stream[A] = if (n == 0) this else this match {
    case Empty => empty
    case Cons(_, t) => t().drop(n - 1)
  }

  def takeWhileV1(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t() takeWhileV1 p)
    case _ => empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = foldRight(empty[A])((a, s) => if (p(a)) cons(a, s) else empty)
  /*
  equivalent
  def takeWhile(p: A => Boolean): Stream[A] = foldRight(empty[A])(infer(p))

  private def infer[A](p: A => Boolean) (a: A, s: => Stream[A]) : Stream[A] = if (p(a)) cons(a, s) else empty

  */
  def forAll(p: A => Boolean): Boolean = !exists(a => !p(a))
  def headOption: Option[A] = foldRight(Option.empty[A])((h, _) => Some(h))
  /*
  equivalent
  def headOption: Option[A] = foldRight(Option.empty[A])(infer)

  private def infer[A] (a: A, s: => Option[A]): Option[A] = this match {
    case Cons(_, _)  => Some(a)
    case _ => s
  }

  */
  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a, s) => cons(f(a), s))
  def mapWithUnfold[B](f: A => B): Stream[B] = unfold(this)(s => s match {
    case Empty => None
    case Cons(h, t) =>
      Some(f(h()), t())
  })

  def filter(predicate: A => Boolean): Stream[A] = foldRight(empty[A])((a, s) => if (predicate(a)) cons(a, s) else s)
  def append[B >: A](b: => Stream[B]): Stream[B] = foldRight(b)((a, s) => cons(a, s))
  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((h, t) => f(h) append t)

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[A](s: Stream[A]): Boolean = ???
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  val onesWithUnfold: Stream[Int] = unfold(1)(i => Some(i, i))


  def constant[A](a: A): Stream[A] = {
    lazy val tail:Stream[A] = Cons(() => a, () => tail)
    // equivalent lazy val tail2:Stream[A] = cons(a, tail2)
    tail
  }
  def constantWithUnfold[A](a: A): Stream[A] = unfold(a)(i => Some(i, i))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))
  def fromWithUnfold (n: Int): Stream[Int] = unfold(n)(i => Some((i, i + 1)))

  def fibs: Stream[Int] = {
    def fibo(first: Int, second:Int): Stream[Int] = cons(first, fibo(second, first + second))

    fibo(0, 1)
  }
  def fibsWithUnfold: Stream[Int] = {
    unfold((0, 1)){ case (n_1:Int, n_2:Int) =>
      lazy val newState = (n_2, n_1 + n_2)
      Some(n_1, newState)
    }

  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty[A]
    case Some((a, s)) => cons(a, unfold(s)(f))
  }

}
object StreamApp extends App {
  val aStream = Stream(1, 2, 3, 4, 5, 6)

  println(aStream.toListRecursive)
  println(aStream.toList)

  println(aStream.toListFast)


  println(aStream.take(2).toList)

  println(aStream.drop(3).toList)

  println(aStream.takeWhile(_ <= 4).toList)

  println(aStream.forAll(_ <= 4))

  println(aStream.forAll(_ <= 7))

  println(aStream.headOption)
  println(aStream.drop(3).headOption)

  println(empty.headOption)

  println(s"Map ${aStream.map(_ * 2).toList}")
  println(s"MapWithUnfold ${aStream.mapWithUnfold(_ * 2).toList}")


  println(aStream.filter(_ % 2 == 0).toList)

  println(aStream.append(cons(7, empty)).toList)

  println(aStream.flatMap(i => cons(i *2, Empty) ).toList)

  println(Stream.ones.take(5).toList)
  println(Stream.onesWithUnfold.take(5).toList)

  println(Stream.constant(3).take(5).toList)
  println(Stream.constantWithUnfold(3).take(5).toList)

  println(Stream.from(3).take(5).toList)
  println(Stream.fromWithUnfold(3).take(5).toList)


  println(Stream.fibs.take(7).toList)

  println(Stream.fibsWithUnfold.take(7).toList)


  println(Stream(1,2,3) startsWith Stream(1,2))
}
