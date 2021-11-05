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

package adpro

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService, Executors}
import scala.language.implicitConversions

// Viktor == 4
// Marouan == 4
// Hristy == 3


// Work through the file top-down, following the exercises from the week's
// sheet.  Uncomment and complete code fragments.

object Par {

  trait Future[+A] {
    private[adpro] def apply(k: A => Unit): Unit
  }

  type Par[A] = ExecutorService => Future[A]

  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref = new java.util.concurrent.atomic.AtomicReference[A]
    val latch = new CountDownLatch(1)
    p(es) { a => ref.set(a); latch.countDown }
    latch.await
    ref.get
  }

  def unit[A](a: A): Par[A] =
    es => new Future[A] {
      def apply(cb: A => Unit): Unit =
        cb(a)
    }

  /** A non-strict version of `unit` */
  def delay[A](a: => A): Par[A] =
    es => new Future[A] {
      def apply(cb: A => Unit): Unit =
        cb(a)
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => new Future[A] {
      def apply(cb: A => Unit): Unit =
        eval(es)(a(es)(cb))
    }

  def async[A](f: (A => Unit) => Unit): Par[A] = es => new Future[A] {
    def apply(k: A => Unit) = f(k)
  }

  def eval (es: ExecutorService)(r: => Unit): Unit =
    es.submit(new Callable[Unit] { def call = r })

  def lazyUnit[A] (a: =>A) : Par[A] = fork(unit(a))

  def map2[A,B,C] (p: Par[A], p2: Par[B]) (f: (A,B) => C): Par[C] =
    es => new Future[C] {
      def apply(cb: C => Unit): Unit = {
        var ar: Option[A] = None
        var br: Option[B] = None
        val combiner = Actor[Either[A,B]](es) {
          case Left(a) =>
            if (br.isDefined) eval(es)(cb(f(a,br.get)))
            else ar = Some(a)
          case Right(b) =>
            if (ar.isDefined) eval(es)(cb(f(ar.get,b)))
            else br = Some(b)
        }
        p(es)(a => combiner ! Left(a))
        p2(es)(b => combiner ! Right(b))
      }
    }

  // map is shown in the blocking part of the book (should still work but
  // probably uses one thread more than the version  below

  // def map[A,B] (pa: Par[A]) (f: A => B) : Par[B] =
  //   map2 (pa,unit (())) ((a,_) => f(a))

  // This is the version of map2 specialized for nonblocking (Section 7.4.4
  // version)

  def map[A,B] (p: Par[A]) (f: A => B): Par[B] =
    es => new Future[B] {
      def apply (cb: B => Unit): Unit =
        p (es) ( a => eval (es) { cb (f (a)) } )
    }

  // Exercise 1
  // Marouan
  // The book introduces the following function on the basic type Par in Section 7.1.1:
  //def unit[A] (a: =>A): Par[A]
  //Why is the argument a is passed by-name to the unit function
  // Because it would be evaluated lazily and therefore ensure that we can run our function in parallel with other functions.
  // Write the answer here in a comment.

  // Exercise 2 (CB7.4)
  // Viktor

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  // Exercise 3
  // Hristy
  //We would test our Par.map function by creating an ExecutorService using the map
  // to map some value to another. Then we would check if the test we gave is equal
  // to the expected output.

  // Write the answer here in a comment.

  // Exercise 4 (CB7.5)
  // Viktor

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight[Par[List[A]]](unit(List.empty))((a, b) => map2(a, b)(_ :: _))
  // this is shown in the book:

  def parMap[A,B] (as: List[A]) (f: A => B): Par[List[B]] =
    sequence (as map (asyncF (f)))

  // Exercise 5
  // Viktor

  def wget(uris: String*): List[String] = {
    val es = Executors.newFixedThreadPool(5)
    run(es)(parMap(uris.toList)(x => scala.io.Source.fromURL(x)("ISO-8859-1").mkString))
  }
  // I need to use the API like this: scala.io.Source.fromURL("https://charouzic.github.io/resume/").mkString

  // the concurrency is achieved by creating value es where we assign the pool of threads (size 5)
  // this way if we call run with the parameter es we tell scala to run the parMap in parallel (on 5 threads)
  // which is evaluating each of the string transformed into list of strings and calling scala.io.Source.fromURL()
  // on each string separately in parallel, and then turning it into a string by calling mkString on the result

  // Exercise 6 (CB7.6)
  // Hristy

  def parFilter[A] (as: List[A]) (f: A => Boolean): Par[List[A]] = {
    map(parMap(as) (aVal => Some(aVal).filter(f)))(aVal => aVal.flatten)
  }

  // shown in the book (adjusted for the non-blocking version)

  def equal[A] (e: ExecutorService) (p: Par[A], p2: Par[A]): Boolean =
    p(e) == p2(e)

  // Exercise 7 (CB7.11)
  // Marouan


  def choiceN[A] (n: Par[Int]) (choices: List[Par[A]]): Par[A] = map2(n, sequence(choices))((x,l) => l(x))

  def choice[A] (n: Par[Boolean]) (t: Par[A], f: Par[A]): Par[A] = choiceN(map(n)(bol => if (bol) 0 else 1))(List(t, f))

  // Exercise 8 (CB7.13)
  // Viktor


  def chooser[A,B] (pa: Par[A]) (choices: A => Par[B]): Par[B] = es => choices(run(es)(pa))(es)

  def choice2[A] (n: Par[Boolean]) (t: Par[A], f: Par[A]): Par[A] = chooser(n)(x => if (x) t else f)

  def choiceN2[A] (n: Par[Int]) (choices: List[Par[A]]): Par[A] = chooser(n)(choices)
  // Exercise 9 (CB7.14)
  // Hristy

  def join[A] (a : Par[Par[A]]) :Par[A] = chooser (a) (a => a)

  // Exercise 10
  // Marouan
  // ...
  implicit class parExtensions[A](val p: Par[A]) extends AnyVal {
    def map[B] (f: A => B): Par[B] = Par.map (p) (f)
    def map2[B,C] (p2: Par[B]) (f: (A,B) => C): Par[C] = Par.map2 (p, p2) (f)
    def chooser[B] (choices: A => Par[B]): Par[B] = Par.chooser (p) (choices)
  }

}
