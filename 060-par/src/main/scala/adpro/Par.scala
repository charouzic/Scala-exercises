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

package adpro

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService, Executors, TimeUnit}
import scala.::
import scala.language.implicitConversions
import scala.io.Source

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
  //
  // Write the answer here in a comment.

  // Exercise 2 (CB7.4)

  def asyncF[A,B] (f: A => B) : A => Par[B] = a => lazyUnit(f(a))


  // Exercise 3
  //
  // Write the answer here in a comment.

  // Exercise 4 (CB7.5)

  def sequence[A] (ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight[Par[List[A]]](unit(List.empty))((a, b) => map2(a, b)(_ :: _))

  // this is shown in the book:

  def parMap[A,B] (as: List[A]) (f: A => B): Par[List[B]] =
    sequence (as map (asyncF (f)))

  // Exercise 5
  //val es = Executors.newFixedThreadPool(5)
  def wget (uris: String*): List[String] = {
    val es = Executors.newFixedThreadPool(5)
    run(es)(parMap(uris.toList)(u => scala.io.Source.fromURL(u)("ISO-8859-1").mkString))
  }

    //uris.map(u => scala.io.Source.fromURL(u)("ISO-8859-1").mkString).toList

  // Exercise 6 (CB7.6)

  def parFilter[A] (as: List[A]) (f: A => Boolean): Par[List[A]] = {
    map(parMap(as) (a => Some(a).filter(f)))(a => a.flatten)
  }

  // shown in the book (adjusted for the non-blocking version)

   def equal[A] (e: ExecutorService) (p: Par[A], p2: Par[A]): Boolean =
     p(e) == p2(e)

  // Exercise 7 (CB7.11)

  def choiceN1[A] (n: Par[Int]) (choices: List[Par[A]]): Par[A] =
    map2(n, sequence(choices))((x, y) => y(x))

  def choice1[A] (n: Par[Boolean]) (t: Par[A], f: Par[A]): Par[A] = {
    choiceN1(map(n)(b => if (b) 0 else 1))(List(t,f))
  }

  // Exercise 8 (CB7.13)

  //def flatMap[A,B] (f: Rand[A]) (g: A => Rand[B]): Rand[B] = {
  //  rng => {
  //    val (a, r) = f(rng)
  //    g(a)(r)
  //  }
  //}

  def chooser[A,B] (pa: Par[A]) (choices: A => Par[B]): Par[B] =
    es => choices(run(es)(pa))(es)

  def choiceN[A] (n: Par[Int]) (choices: List[Par[A]]): Par[A] =
    chooser(n)(choices)

  def choice[A] (n: Par[Boolean]) (t: Par[A], f: Par[A]): Par[A] = {
    chooser(n)(x => if (x) t else f)
  }

  // Exercise 9 (CB7.14)

  def join[A] (a : Par[Par[A]]) :Par[A] = chooser(a)(a => a)

  // Exercise 10
  //
  // ...

}
