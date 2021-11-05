// Advanced Programming, A. Wąsowski, IT University of Copenhagen
//
// Group: ____________
// AUTHOR1: Viktor 6
// AUTHOR2: Marouan 5
// AUTHOR3: Hristy  5

package adpro

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {

  case class SimpleRNG (seed: Long) extends RNG {

    def nextInt: (Int, RNG) = {

      // `&` is bitwise AND. We use the current seed to generate a new seed.
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL

      // The next state, which is an `RNG` instance created from the new seed.
      val nextRNG = SimpleRNG (newSeed)

      // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      val n = (newSeed >>> 16).toInt

      // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
      (n, nextRNG)

    }

  }

  // Exercise 1 (CB 6.1)
  // Marouan

  def nonNegativeInt (rng: RNG): (Int, RNG) = {
    val (i,r) = rng.nextInt
    (if (i < 0) -(i+1) else i,r)
  }

  // Exercise 2 (CB 6.2)
  // Hristy

  def double (rng: RNG): (Double, RNG) = {
    val (i,r) = nonNegativeInt(rng)
    (i/(Int.MaxValue.toDouble + 1),r)
  }
  // Exercise 3 (CB 6.3)
  // Marouan

  def intDouble (rng: RNG) : ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt (rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def boolean (rng: RNG): (Boolean, RNG) =
    rng.nextInt match { case (i,rng2) => (i % 2 == 0, rng2) }

  // Exercise 4 (CB 6.4)
  // Viktor

  def ints (count: Int) (rng: RNG): (List[Int],RNG) =
    if (count == 0) (List(), rng)
    else {
      val (i, rng2) = rng.nextInt
      val (l, rng3) = ints(count-1)(rng2)
      (i :: l, rng3)
    }
  // There is something terribly repetitive about passing the RNG along
  // every time. What could we do to eliminate some of this duplication
  // of effort?

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A] (a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B] (s: Rand[A]) (f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s (rng)
      (f (a), rng2)
    }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt) (i => i - i % 2)

  // Exercise 5 (CB 6.5) (Lazy is added so that the class does not fail
  // at load-time without your implementation).
  // Hristy

  lazy val _double: Rand[Double] = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

  // Exercise 6 (CB 6.6)
  // Marouan

  def map2[A,B,C] (ra: Rand[A], rb: Rand[B]) (f: (A, B) => C): Rand[C] = rng => {
    val (a, r1) = ra(rng)
    val (b, r2) = rb(r1)
    (f(a, b), r2)
  }

  // this is given in the book

  def both[A,B] (ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2 (ra, rb) ((_, _))

  lazy val randIntDouble: Rand[(Int, Double)] = both (int, double)

  lazy val randDoubleInt: Rand[(Double, Int)] = both (double, int)

  // Exercise 7 (6.7)
  // Viktor

  // def sequence[A] (aos: List[Option[A]]): Option[List[A]] = {
  //    aos.foldRight[Option[List[A]]](Some(Nil))((a,b)=>map2(a,b)(_::_))

  def sequence[A] (fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((a,b)=>map2(a,b)(_::_))

  def _ints (count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))


  // Exercise 8 (6.8)
  // Hristy

  def flatMap[A,B] (f: Rand[A]) (g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, r) = f(rng)
      g(a)(r)
    }
  }

  def nonNegativeLessThan (n: Int): Rand[Int] =  { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n-1) - mod >= 0)
      (mod, rng2)
    else nonNegativeLessThan(n)(rng)
  }

}

import State._

case class State[S, +A](run: S => (A, S)) {

  // Exercise 9 (6.10)
  // Viktor

  def map[B] (f: A => B): State[S, B] = flatMap(a => unit(f(a)))

  def map2[B,C] (sb: State[S, B]) (f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a,b)))

  def flatMap[B] (f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })

}

object State {

  import adpro.Stream

  type Rand[A] = State[RNG, A]

  def unit[S, A] (a: A): State[S, A] =
    State (s => (a, s))

  // Exercise 9 (6.10) continued
  // Marouan

  def sequence[S,A] (sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(unit[S, List[A]](List()))((f, acc) => f.map2(acc)(_ :: _))

  // This is given in the book:

  def modify[S] (f: S => S): State[S, Unit] = for {
    // Get the current state and assigns it to `s`.
    s <- get
    // Set the new state to `f` applied to `s`.
    _ <- set (f (s))
  } yield ()

  def get[S]: State[S, S] = State (s => (s, s))

  def set[S] (s: S): State[S, Unit] = State (_ => ((), s))

  def random_int: Rand[Int] =  State (_.nextInt)

  // Exercise 10
  // Hristy

  def state2stream[S,A] (s: State[S,A]) (seed: S): Stream[A] = {
    val (a, seed2) = s.run(seed)
    Cons(() => a, () => state2stream(s)(seed2))
  }

  // Exercise 11 (lazy is added so that the class does not crash at load time
  // before you provide an implementation).
  // Hristy

  lazy val random_integers = state2stream(random_int)(RNG.SimpleRNG(10))

}
