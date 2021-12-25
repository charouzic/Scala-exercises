// Advanced Programming, Exercises by A. Wąsowski, IT University of Copenhagen
//
// Work on this file by following the associated exercise sheet
// (available in PDF in the same directory).
//
// The file is meant to be compiled inside sbt, using the 'compile' command.
// To run the compiled file use the 'run' or 'runMain'.
// To load the file int the REPL use the 'console' command.
//
// Continue solving exercises in the order presented in the PDF file. The file
// shall always compile, run, and pass tests (for the solved exercises),
// after you are done with each exercise (if you do them in order).
// Compile and test frequently. Best continously.

package fpinscala

object Exercises extends App with ExercisesInterface {

  import fpinscala.List._

  // Exercise 1 requires no programming

  // Exercise 2

  def tail[A] (as: List[A]) :List[A] = ???

  // Exercise 3

  // @annotation.tailrec
  // Uncommment the annotation after solving to make the
  // compiler check whether you made the solution tail recursive
  def drop[A] (l: List[A], n: Int) : List[A] = ???

  // Exercise 4

  def dropWhile[A] (l: List[A], f: A => Boolean): List[A] = ???

  // Exercise 5

  def init[A] (l: List[A]): List[A] = ???

  // Exercise 6

  def length[A] (as: List[A]): Int = ???

  // Exercise 7

  // @annotation.tailrec
  // Uncommment the annotation after solving to make the
  // compiler check whether you made the solution tail recursive
  def foldLeft[A,B] (as: List[A], z: B) (f: (B, A) => B): B = ???

  // Exercise 8

  def product (as: List[Int]): Int = ???

  def length1[A] (as: List[A]): Int = ???

  // Exercise 9

  def reverse[A] (as: List[A]): List[A] = ???

  // Exercise 10

  def foldRight1[A,B] (as: List[A], z: B) (f: (A, B) => B): B = ???

  // Exercise 11

  def foldLeft1[A,B] (as: List[A], z: B) (f: (B,A) => B): B = ???

  // Exercise 12

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
  }

  def concat[A] (as: List[List[A]]): List[A] = ???

  // Exercise 13

  def filter[A] (as: List[A]) (p: A => Boolean): List[A] = ???

  // Exercise 14

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = ???

  // Exercise 15

  def filter1[A] (l: List[A]) (p: A => Boolean) :List[A] = ???

  // Exercise 16

  def add (l: List[Int]) (r: List[Int]): List[Int] = ???

  // Exercise 17

  def zipWith[A,B,C] (f: (A,B)=>C) (l: List[A], r: List[B]): List[C] = ???

  // Exercise 18

  def hasSubsequence[A] (sup: List[A], sub: List[A]): Boolean = ???

}
