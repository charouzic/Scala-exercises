// Advanced Programming
// Andrzej Wąsowski, IT University of Copenhagen

// Hristiyana ==> 7
// Marouan ==> 8
// Viktor ==> 8

package adpro

import com.sun.source.doctree.SeeTree

import java.util.stream

sealed trait Stream[+A] {
  import Stream._

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons (h, t) => Some(h ())
  }

  def tail: Stream[A] = this match {
    case Empty => Empty
    case Cons (h, t) => t ()
  }

  def foldRight[B] (z : =>B) (f: (A, =>B) => B): B = this match {
    case Empty => z
    case Cons (h, t) => f (h (), t ().foldRight (z) (f))
    // Note 1. f can return without forcing the tail
    // Note 2. this is not tail recursive (stack-safe) It uses a lot of stack
    // if f requires to go deeply into the stream. So folds sometimes may be
    // less useful than in the strict case
  }

  // Note. foldLeft is eager; cannot be used to work with infinite streams. So
  // foldRight is more useful with streams (somewhat opposite to strict lists)
  def foldLeft[B] (z: =>B) (f: (A, =>B) =>B): B = this match {
    case Empty => z
    case Cons (h,t) => t().foldLeft (f (h (), z)) (f)
    // Note 2. even if f does not force z, foldLeft will continue to recurse
  }

  def exists (p : A => Boolean): Boolean = this match {
    case Empty => false
    case Cons (h, t) => p (h ()) || t ().exists (p)
    // Note 1. lazy; tail is never forced if satisfying element found this is
    // because || is non-strict
    // Note 2. this is also tail recursive (because of the special semantics of ||)
  }

  // Exercise 2
  // Hristiyana

  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A],acc: List[A]): List[A] = s match {
      case Cons(hd,tl) => go(tl(),hd()::acc)
      case _ => acc
    }
    go(this, List()).reverse
  }

  // Exercise 3
  // Hristiyana
  
  
  def take (n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)  
    case _ => empty

  }

  @annotation.tailrec
  final def drop (n: Int): Stream[A] = this match {
    case Cons(_, tl) if n > 0 => tl() drop(n - 1)
    case _ => this
  }

  /*def take (n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def drop (n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }*/

  // Exercise 4
  // Hristiyana

  def takeWhile (p: A => Boolean): Stream[A] = this match {
    case Cons(hd, tl) if p(hd()) => cons(hd(),tl() takeWhile p) 
    case _ => empty
  }

  //Exercise 5
  // Viktor

  def forAll (p: A => Boolean): Boolean =
    this.foldRight(false)((h, t) => p(h) && t)

  // crashing for naturals.forAll (_ >= 0) as naturals return stream starting from 1 therefore
  // it would never stop

  // it's not good practice to use exists() or forAll() on infinite streams without knowing the result
  // as it would keep evaluating next and next item in the stream potentially never finding it
  // therefore using exist() or forAll() on finite streams is similar to using them on lists where
  // iterating over items in the stream eventually goes to an end


  //Exercise 6
  // Marouan
  
  def takeWhile2 (p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) =>
      if (p(h)) cons(h,t)
      else empty)


  //Exercise 7
  // Viktor

  def headOption2: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  //Exercise 8 The types of these functions are omitted as they are a part of the exercises
  // Viktor

  def map[B](f: A=>B): Stream[B] = {
    //def frh(a: A, b: =>Stream[B]): Stream[B] = cons(f(a), b)
    //foldRight[Stream[B]] (Empty)(frh _)
    foldRight(empty[B])((h,t) => cons(f(h), t))
  }

  def filter(f: A=>Boolean):Stream[A] =
    foldRight(empty[A])((h,t) => if (f(h)) cons(h,t) else t)

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h,t) => cons(h,t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h,t) => f(h) append t)

  //Exercise 09
  // Viktor
  //Put your answer here:
  // It is better for memory management as the filter transforms the whole stream but since
  // streams are evaluated lazily, only the value that fits our purposes will be computed and returned
  // (find terminates at the moment when the match is found).
  // Streams use the memory for the current value whereas lists need each elemont in memory.

  // Exercise 13
  // Marouan

  def map_[B] (f: A=>B): Stream[B] = unfold(this){
    case Cons(h, t) => Some(f(h()),t())
    case Empty => None
  }

  def take_  (n: Int): Stream[A] = unfold((this,n)){
    case (Cons(h, t),n) if n > 1 => Some(h(), (t(),n-1))
    case (Cons(h, _),n) if n == 1 => Some(h(), (Empty,0))
    case _ => None
  }

  def takeWhile_ (p: A => Boolean): Stream[A] = unfold(this){
    case Cons(h, t) if p(h()) => Some(h(), t())
    case _ => None
  }

  def zipWith_ [B,C](f: (A,B)=> C)(s: Stream[B]): Stream[C] = unfold((this,s)){
    case (Cons(hd1, tl1),Cons(hd2,tl2)) => Some(f(hd1(), hd2()), (tl1(),tl2()))
    case _ => None
  }

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
  // Hristiyana
  def from (n: Int): Stream[Int] = cons(n,from(n+1))

  def to (n: Int): Stream[Int] = cons(n,from(n-1))

  val naturals: Stream[Int] = from(1)

  
/*  def from (n: Int): Stream[Int] = Stream.cons(n, from(n+1))

  def to (n: Int): Stream[Int] = {
    if (n == 1) Stream.cons(1, Stream.empty)
    else Stream.cons(n, to(n-1))
  }
*/
  
  //Exercise 10
  // Viktor
  //Put your answer here:

  lazy val fibs = {
    def go(f: BigInt, s: BigInt): Stream[BigInt] = {
      cons(f,go(s, f+s))
    }
    go(0,1)
  }

  //Exercise 11
  // Marouan

  def unfold [A, S] (z: S) (f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((hd,s)) => cons(hd, unfold(s)(f))
    case None => empty
  }

  // Exercise 12
  // Marouan

  def fibs1  = unfold((0,1)){ case (f,s) => Some((f,(s,f+s))) }
  
  def from1 (n: Int) = unfold (n) { case (v) => Option(v, v+1) }

}

