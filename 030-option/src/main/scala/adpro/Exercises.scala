// Advanced Programming, A. Wąsowski, IT University of Copenhagen
//
// Group number: _____
//
// AUTHOR1: __________
// TIME1: _____ <- how much time have you used on solving this exercise set
// (excluding reading the book, fetching pizza, and going out for a smoke)
//
// AUTHOR2: __________
// TIME2: _____ <- how much time have you used on solving this exercise set
// (excluding reading the book, fetching pizza, and going out for a smoke)
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

  override def compare (that: java.awt.Point): Int =  ???

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

  def size[A] (t: Tree[A]): Int = ???

  // Exercise 3

  def maximum (t: Tree[Int]): Int = ???

  // Exercise 4

  def map[A,B] (t: Tree[A]) (f: A => B): Tree[B] = ???

  // Exercise 5

  def fold[A,B] (t: Tree[A]) (f: (B,B) => B) (g: A => B): B = ???

  def size1[A] (t: Tree[A]): Int = ???

  def maximum1 (t: Tree[Int]): Int = ???

  def map1[A,B] (t: Tree[A]) (f: A=>B): Tree[B] = ???

}

sealed trait Option[+A] {

  // Exercise 6

  def map[B] (f: A=>B): Option[B] = ???

  /**
   * Ignore the arrow (=>) in default's type below for now.
   * It prevents the argument "default" from being evaluated until it is needed.
   * So it is not evaluated if we process a Some object (this is 'call-by-name'
   * and we should talk about this soon).
   */

  def getOrElse[B >: A] (default: => B): B = ???

  def flatMap[B] (f: A => Option[B]): Option[B] = ???

  def filter (p: A => Boolean): Option[A] = ???

}

case class Some[+A] (get: A) extends Option[A]
case object None extends Option[Nothing]

object ExercisesOption {

  // mean is implemented in Chapter 4 of the text book

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some (xs.sum / xs.length)

  // Exercise 7

  def headOption[A] (lst: List[A]): Option[A] = lst match {
    case Nil => None
    case h ::t => Some (h)
  }

  def headGrade (lst: List[(String,Int)]): Option[Int] = ???

  // Exercise 8

  def variance (xs: Seq[Double]): Option[Double] = ???

  // Exercise 9

  def map2[A,B,C] (ao: Option[A], bo: Option[B]) (f: (A,B) => C): Option[C] = ???

  // Exercise 10

  def sequence[A] (aos: List[Option[A]]): Option[List[A]] = ???

  // Exercise 11

  def traverse[A,B] (as: List[A]) (f: A => Option[B]): Option[List[B]] = ???

}
