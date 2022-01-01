// Advanced Programming
// Andrzej Wąsowski, IT University of Copenhagen

package adpro

sealed trait Stream[+A] {

  import Stream._

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def tail: Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) => t()
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Empty => z
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    // Note 1. f can return without forcing the tail
    // Note 2. this is not tail recursive (stack-safe) It uses a lot of stack
    // if f requires to go deeply into the stream. So folds sometimes may be
    // less useful than in the strict case
  }

  // Note. foldLeft is eager; cannot be used to work with infinite streams. So
  // foldRight is more useful with streams (somewhat opposite to strict lists)
  def foldLeft[B](z: => B)(f: (A, => B) => B): B = this match {
    case Empty => z
    case Cons(h, t) => t().foldLeft(f(h(), z))(f)
    // Note 2. even if f does not force z, foldLeft will continue to recurse
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Empty => false
    case Cons(h, t) => p(h()) || t().exists(p)
    // Note 1. lazy; tail is never forced if satisfying element found this is
    // because || is non-strict
    // Note 2. this is also tail recursive (because of the special semantics of ||)
  }

  // Exercise 2

  def toList: List[A] = {
    def go(s: Stream[A], l: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: l)
      case _ => l
    }

    go(this, List()).reverse
  }


  // Exercise 3

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => t().drop(n - 1)
    case Cons(_, t) if n == 1 => t()
    case _ => empty
  }

  // Exercise 4

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }


  //Exercise 5

  def forAll (p: A => Boolean): Boolean = {
    def go(s: Stream[A]): Boolean = s match {
      case Cons(h, t) if p(h()) => go(t())
      case _ => false
    }
    go(this)
  }


  //Exercise 6

  def takeWhile2 (p: A => Boolean): Stream[A] =
    this.foldRight(empty[A])((h, t)  =>  if (p(h)) cons(h, t) else empty)


  //Exercise 7

  def headOption2: Option[A] =
    this.foldRight(None: Option[A])((h, _) => Some(h))

  //Exercise 8 The types of these functions are omitted as they are a part of the exercises

  def map[B](f: A=>B): Stream[B] =
    this.foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(f: A=>Boolean):Stream[A] =
    this.foldRight(empty[A])((h, t) => if (f(h)) cons(h, t) else t)

  def append[B>:A](s: => Stream[B]): Stream[B] =
    this.foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    this.foldRight(empty[B])((h,t) => f(h) append t)

  //Exercise 09
  //Put your answer here:

  // Exercise 13

  def map_ = ???
  def take_ = ???
  def takeWhile_ = ???
  def zipWith_ = ???
}


case object Empty extends Stream[Nothing]
case class Cons[+A] (h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def empty[A]: Stream[A] = Empty

  def cons[A] (hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons (() => head, () => tail)
  }

  def apply[A] (as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons (as.head, apply (as.tail: _*))
    // Note 1: ":_*" tells Scala to treat a list as multiple params
    // Note 2: pattern matching with :: does not seem to work with Seq, so we
    //         use a generic function API of Seq


  // Exercise 1

  def from (n: Int): Stream[Int] = cons(n, from(n+1))


  def to (n: Int): Stream[Int] = cons(n, to(n-1))

  val naturals: Stream[Int] = from(1)

  //Exercise 10
  //Put your answer here:
  lazy val fibs = {
    def go(f: BigInt, s: BigInt): Stream[BigInt] = {
      cons(f,go(s, f+s))
    }
    go(0,1)
  }

  //Exercise 11

  def unfold [A, S] (z: S) (f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((hd,s)) => cons(hd, unfold(s)(f))
    case None => empty
  }

  // Exercise 12

  def fibs1  = unfold((0,1)){ case (f,s) => Some((f,(s,f+s))) }

  def from1 (n: Int) = unfold (n) { case (v) => Option(v, v+1) }

}
