// Advanced Programming, A. Wąsowski, IT University of Copenhagen

// Viktor   9
// Hristy   9
// Marouan  9
package adpro

import fpinscala.laziness.Stream._
import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.state.RNG._


object WarmupExercises {


  // Exercise 1
  // Viktor
  lazy val rng1: RNG = RNG.Simple(42)


  // Exercise 2
  // Viktor
  val (number, rng2) = rng1.nextInt
  lazy val x: Int = number
  lazy val y: Int = rng2.nextInt._1

  // Exercise 3
  // Hristy
  lazy val s_random_int: State[RNG,Int] = ???
  lazy val s_nonNegativeInt: State[RNG,Int] = ???
  lazy val s_double: State[RNG,Double] = ???

  lazy val random_int: Int =  ???
  lazy val nonNegativeInt: Int =  ???
  lazy val double: Double = ???

  import Gen.state2stream

  // Exercise 4
  // Marouan
  def randomDoubles (seed: RNG): Stream[Double] = ???

  lazy val someRandomDoubles: List[Double] = ???
  lazy val moreRandomDoubles: List[Double] = ???

  // Exercise 5
  // Marouan
  def impureRandomDoubles: Stream[Double] = ???

  lazy val impureDoubles1: Stream[Double] = ???
  lazy val impureDoubles2: Stream[Double] = ???

}

case class Gen[A] (sample: State[RNG,A]) {

  // Let's convert generator to streams of generators
  def toStream (seed: Long): Stream[A] =
    Gen.state2stream (this.sample) (RNG.Simple (seed))

  def toStream (rng: RNG): Stream[A] =
    Gen.state2stream (this.sample) (rng)

  // Exercise 8
  // Hristy

  def listOfN (n: Int): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(this.sample)))

  // Exercise 9
  // Marouan

  def flatMap[B] (f: A => Gen[B]): Gen[B] = Gen(this.sample.flatMap(x => f(x).sample))

  // It would be convenient to also have map  (uses flatMap)

  def map[B] (f: A => B): Gen[B] =
    this.flatMap (a => Gen.unit[B] (f(a)))

  // Exercise 10
  // Viktor

  def listOf (size: Gen[Int]): Gen[List[A]] =
    size flatMap (n => this.listOfN(n))

  // Exercise 11
  // Hristy

  def union (that: Gen[A]): Gen[A] = ???

  // Exercise 12 continues in the companion object (below)
  // Viktor
}

object Gen {

  // A convenience function to convert states (automata) to streams (traces)
  // It would be better to have it in State, but I am not controlling
  // State.scala.

  private[adpro] def state2stream[A] (s: State[RNG,A]) (seed: RNG): Stream[A] =
    s.run (seed) match { case (n,s1) => cons (n, state2stream (s) (s1)) }

  // A generator for Integer instances

  def anyInteger: Gen[Int] =
    Gen (State (_.nextInt))

  // Exercise 6
  // Viktor

  def choose (start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive-start)))

  // Exercise 7

  // Hristy
  def unit[A] (a: => A): Gen[A] =
    Gen(State.unit(a))

  // Marouan
  def boolean: Gen[Boolean] = ???

  // Viktor
  def double: Gen[Double] =
    Gen(State(RNG.double))

  // (Exercise 8 is found in the Gen class above)
  // Hristy

  // Exercise 13
  // Viktor

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen[List[A]](State.sequence(List.fill(n)(g.sample)))

  def listOf[A](size: Int, g: Gen[A]): Gen[List[A]] =
    listOfN(size, g)
    // size flatMap (n => this.listOfN(n))

  // Adapt the tests from Exercise 8 and 10 to test the answers. The tests provided
  // for Ex 13 only check types, not functionality.

}

// This is the Prop type implemented in [Chiusano, Bjarnasson 2015]

object Prop {

  type TestCases = Int
  type SuccessCount = Int
  type FailedCase = String

  // the type of results returned by property testing

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified = false
  }

  case class Falsified (
                         failure: FailedCase,
                         successes: SuccessCount
                       ) extends Result {
    def isFalsified = true
  }

  case object Proved extends Result {
    def isFalsified = false
  }

  def forAll[A] (as: Gen[A]) (f: A => Boolean): Prop = Prop {

    (n, rng) => as.toStream (rng).zip (Stream.from (0)).take (n).map {
      case (a, i) => try {
        if (f (a)) Passed else Falsified (a.toString, i)
      } catch { case e: Exception => Falsified (buildMsg (a, e), i) }

    }.find (_.isFalsified).getOrElse (Passed)
  }

  def buildMsg[A] (s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString ("\n")}"
}

import Prop._

case class Prop (run: (TestCases, RNG) => Result) {

  // (Exercise 12)
  // Viktor

  def && (that: Prop): Prop = Prop {
    (n, rng) => run(n, rng) match {
      case Passed => that.run(n, rng)
      case Proved => that.run(n, rng)
      case result => result
    }
  }

  def ||(that: Prop): Prop = Prop {
    (n,rng) => {
      this.run(n, rng) match {
        case Falsified(_,_) => that.run(n,rng)
        case x => x
      }
    }
  }

  // Exercise 13 is in the companion object of Gen
  // Viktor

}

// vim:cc=80:foldmethod=indent:nofoldenable