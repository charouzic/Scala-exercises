// Advanced Programming, A. Wąsowski, IT University of Copenhagen
//
// Group number: Hand-In Group Y
//
// AUTHOR1: Viktor Macek
// TIME1: 4 hours
//
// AUTHOR2: Hristiyana Toteva
// TIME2: 4 hours
//
// AUTHOR3: Marouan El Haddad
// TIME2: 4 hours
//
// You should work with the file by following the associated exercise sheet
// (available in PDF from the course website).
//
// The file shall always compile and run after you are done with each exercise
// (if you do them in order).  Please compile and test frequently. Of course,
// some tests will be failing until you finish. Only hand in a solution that
// compiles and where tests pass for all parts that you finished.  The tests
// will fail for unfnished parts.  Comment such out.

package adpro

// Exercise  1

trait OrderedPoint extends scala.math.Ordered[java.awt.Point] {

  this: java.awt.Point =>

  override def compare (that: java.awt.Point): Int =
    if (getX == that.getX) getY compare that.getY
    else getX compare that.getX
  }

// Try the following (and similar) tests in the repl (sbt console):
// val p = new java.awt.Point(0,1) with OrderedPoint
// val q = new java.awt.Point(0,2) with OrderedPoint
// assert(p < q)

sealed trait Tree[+A]
case class Leaf[A] (value: A) extends Tree[A]
case class Branch[A] (left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // Exercise 2

  def size[A] (t: Tree[A]): Int = t match {
      case Leaf(value) => 1
      case Branch(left, right) => 1 + size(left) + size(right)
  }

  // Exercise 3

  def maximum (t: Tree[Int]): Int = t match {
    case Leaf(x) => x
    case Branch(left, right) => maximum(left).max(maximum(right))
  }

  // Exercise 4

  def map[A,B] (t: Tree[A]) (f: A => B): Tree[B] = t match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  // Exercise 5

  def fold[A,B] (t: Tree[A]) (f: (B,B) => B) (g: A => B): B = t match {
    case Leaf(a) => g(a)
    case Branch(left,right) => f(fold(left)(f)(g), fold(right)(f)(g))
  }

  def size1[A] (t: Tree[A]): Int = 
    fold[A, Int](t)((l,r)=>1+l+r)(le=>1)

  def maximum1 (t: Tree[Int]): Int =
    fold(t)((x1: Int, x2:Int) => x1.max(x2))(a => a)

  def map1[A,B] (t: Tree[A]) (f: A=>B): Tree[B] = 
    fold[A,Tree[B]](t)((l,r) => Branch(l,r))(v=>Leaf(f(v)))

}

sealed trait Option[+A] {

  // Exercise 6

  def map[B] (f: A=>B): Option[B] = this match {
    case None => None
    case Some(get) => Some(f(get))
  }


  /**
   * Ignore the arrow (=>) in default's type below for now.
   * It prevents the argument "default" from being evaluated until it is needed.
   * So it is not evaluated if we process a Some object (this is 'call-by-name'
   * and we should talk about this soon).
   */


  def getOrElse[B >: A] (default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B] (f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(get) => f(get)
  }

  def filter (p: A => Boolean): Option[A] = this match {
    case Some(x) if p(x) => this
    case _ => None
  }
}

case class Some[+A] (get: A) extends Option[A]
case object None extends Option[Nothing]

object ExercisesOption {

  // mean is implemented in Chapter 4 of the text book

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some (xs.sum / xs.length)

  // Exercise 7

  val x = list(("sda", 12), ("sda", 12),("sda", 12))
  def headOption[A] (lst: List[A]): Option[A] = lst match {
    case Nil => None
    case h ::t => Some (h)
  }

  def headGrade (lst: List[(String,Int)]): Option[Int] =
    headOption (lst map (x => x._2))
  // for- yield comprehension
  /*for {
    x <- headOption(lst)
  } yield (x._2)
     */



  // Exercise 8

  def variance (xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => 
      mean(xs.map(x=>math.pow(x-m,2))))
  }
  
  def variance2 (xs: Seq[Double]): Option[Double] = {
      for {
        m <- mean(xs)
        a <- mean(xs.map(x=>math.pow(x-m,2)))
      } yield a
    }


  // Exercise 9

  def map2[A,B,C] (ao: Option[A], bo: Option[B]) (f: (A,B) => C): Option[C] = {
  ao.flatMap (aa => 
    bo.map (bb => 
      f(aa,bb)))
  }

  def map2yield[A,B,C] (ao: Option[A], bo: Option[B]) (f: (A,B) => C): Option[C] = {
    for {
      aa <- ao
      x <- bo.map(bb => f(aa,bb))
    } yield x
  }


  // Exercise 10

  def sequence[A] (aos: List[Option[A]]): Option[List[A]] = {
    aos.foldRight[Option[List[A]]](Some(Nil))((a,b)=>map2(a,b)(_::_))
    //  (_::_) == cons(_, _)
  }


  // Exercise 11

  def traverse[A,B] (as: List[A]) (f: A => Option[B]): Option[List[B]] = 
    as.foldRight[Option[List[B]]](Some(Nil))((hd,tl) => map2(f(hd),tl)(_ :: _))

}
