// Advanced Programming Andrzej Wasowski, IT University of Copenhagen
//
// Monocle is a library providing lenses for Scala (one of several in fact)
//
// A Tutorial and other documentation for Monocle lenses is here:
// https://www.optics.dev/Monocle/
//
// We will reimplement some Lenses today, but we shall reuse some basic
// infrastructure from Monocle.  Monocle is *probably* the most popular Lens
// framework for scala.
//
// Some examples can be found here (and in other files in the same directory):
// https://www.optics.dev/Monocle/docs/examples/university_example.html
//
// Work through the file below in the order of numbered exercises (top to
// bottom), referring to LensesSpec.scala whenever necessary.
//
// Some notes to keep in mind when working and reading:
//
// 1. Put is called 'replace' in Monocle
//
// 2. Put/Set/Replace is curried ([Morries 2012] has a discussion that touches on
//    advantages of currying 'set')
//
// 3. Total lenses are called Lens in monocle.  Partial lenses are of type
//    Optional.

package adpro

import monocle._
import monocle.syntax.all._
import monocle.function.all._
import monocle.PLens.lensChoice

import org.scalacheck.Arbitrary
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks._
import org.scalatest.matchers.should.Matchers._

// Used by 'each' in monocle
import cats.Order
import cats.implicits.catsKernelStdOrderForString


object Lenses {

  // Exercise 1
  //
  // Study the implementation of lens l1 below and compare it to the
  // first example in Foster et al. (Page 6).

  val l1 = Lens[(String,Int), String] (get = _._1) (replace = s1 => _ => (s1, 0))

  // Complete the second example from page 6, and the example from page 7 below:

  lazy val l2: Lens[String, (String,Int)] = ???

  // Finally the example from page 7 in Foster et al.:

  lazy val l3: Lens[(String,Int), String] = ???

  // We will test these implementations in Exercise 3.  For now, we are
  // satisfied if they type check and compile.

  object Laws {

    // Exercise 2

    // Write the PutGet law as a property test for arbitrary lenses from
    // type C to typ A.

    def PutGet[C: Arbitrary, A: Arbitrary] (l: Lens[C,A]) = ???

    // Write the GetPut law:

    def GetPut[C: Arbitrary, A] (l: Lens[C,A]) = ???

    // Write the PutPut law:

    def PutPut[C: Arbitrary, A: Arbitrary] (l: Lens[C,A]) = ???

    // Exercise 3 continues below in Exercise3Spec, when solving it you can use
    // the two aggregations of laws defined below (cf. Foster et al.):

    def wellBehavedTotalLense[A: Arbitrary, C: Arbitrary] (l: Lens[C, A]) = {

      withClue ("PutGet law: ") { PutGet (l) }
      withClue ("GetPut law: ") { GetPut (l) }
    }

    def veryWellBehavedTotalLense[A: Arbitrary, C: Arbitrary] (l: Lens[C,A]) = {
        withClue ("Well behaved total lense: ") {
          wellBehavedTotalLense (l)
        }
        withClue ("PutPut law: ") {
          PutPut (l)
        }
    }

  }


  class Exercise3Spec
    extends org.scalatest.freespec.AnyFreeSpec
    with org.scalatest.matchers.should.Matchers
    with org.scalatestplus.scalacheck.ScalaCheckPropertyChecks {

    // Exercise 3

    // Test  lenses l1, l2, and l3 using the general laws implemented above. Check
    // with the paper whether the results are consistent.

    // l1 fails GetPut (see Foster). Check to convince yourself that it fails and
    // submit only the PutGet and PutPut tests.

    "Exercise 3 (testing Exercise 1, l1)" in {
      ???
    }

    // l2 fails PutGet see p. 6 in Foster (check to convince yourself that it
    // fails, and submit only the GetPut and PutPut test.

    "Exercise 3 (testing Exercise 1, l2)" in {
      ???
    }

    // l3 fails PutPut (check yourself and submit only the tests for the other
    // laws)

    "Exercise 3 (testing Exercise 1, l3)" in {
      ???
    }

  }

  // Exercise 4
  //
  // Implement the lense codiag from Either[A,A] to A (this is
  // presented by [Morris, 2012]. This may be thought of as taking either A or A
  // and stripping the choice of A from the Either value. The type of this value
  // is Lens[Either[A, A], A].

  // (ca 10 lines of code)

  // We have automatic tests for codiag are found in LensesSpec to check your
  // solution.

  def codiag[A]: Lens[Either[A,A], A] = ???

  // Exercise 5
  //
  // Section 5.3 in Morris's paper [Morris  2012] describes a choice combinator
  // for lenses |||: Lens[R, F] => Lens[S, F] => Lens[Either[R, S], F].
  //
  // Morris uses it to implement the above codiag lense together with an
  // identity lense (Identity is described in [Foster et al. p 12] and in
  // [Morris p.3 Listing 12].
  //
  // In Monocle '|||' is called "lensChoice.choice" and identity is called
  // "Iso.id". Observe also that Monocle's replace is curried, while Morris's
  // setters are not.
  //
  // Translate Morris's implementation of codiag to Monocle. There are automated
  // tests in the test suite for this exercise.

  // (ca. 1 line)

  def codiag1[A]: Lens[Either[A, A], A] = ???

  // Exercise 6
  //
  // Important: this exercise shows the main application of lenses
  // Consider the following types:

  type ZipCode = String
  type Name = String
  type Students = Map[Name, Address]

  case class Address (
    val zipcode: ZipCode,
    val country: String
  )

  case class University (
    val students: Students,
    val address: Address
  )

  val itu = University (

    students = Map[Name, Address] (
      "Stefan"    -> Address ("2300",   "Denmark"),
      "Axel"      -> Address ("91000",  "France"),
      "Alex"      -> Address ("2800",   "Denmark"),
      "Christian" -> Address ("D-4242", "Germany"),
      "Andrzej"   -> Address ("00-950", "Poland"),
      "Thorsten"  -> Address ("6767",   "Sweden")
    ),

    address = Address ("2300", "Amager")

  )

  // Write an expression that modifies "itu" in such a way that Alex is in
  // Denmark but at post-code 9100. First do it *without* using lenses.

  // Hint: every class in Scala has a method called 'copy' that takes the same
  // parameters as the constructor.  All parameters are optional.  Use the name
  // assignment convention to only change values of properties that you want in
  // the copy.  For instance itu.copy (students = itu.students.tail) creates a
  // copy of ITU without the first student.
  //
  // Reflect how inconvenient it is, even with the copy method. Notice, how easy
  // and natural this change would be in an imperative style (for instance in C#
  // or Java).
  //
  // There is a test in LensesSpec to check whether  you did what expected.

  lazy val itu1: University = ???

  // As you see doing this without lenses is very very annoying.  Updating
  // nested properties in complex objects is much easier in imperative
  // programming.

  // Exercise 7
  //
  // Lenses to the rescue.  Extend our hypothetical university library with
  // lenses, so that using the types is almost as natural as in imperative
  // languages.
  //
  // a) design a lense that accesses zipcode from Address objects:
  //  (1-2 lines)

  lazy val _zipcode: Lens[Address, ZipCode] = ???

  // b) design a lense that accesses the students collection from university:
  //  (1-2 lines)

  lazy val _students: Lens[University,Students] = ???

  // c) Use the following partial lense 'index(name)' from Monocle:
  //
  // index (name): Optional[SortedMap[String,Address], Address]
  //
  // This lens focuses the view on the entry in a map with a given index.
  // Optional in the Monocle terminology is the same a partial lense in the
  // terminology of Foster et al.
  //
  // Use lenses composition to update itu the same way (move Alex to zipcode
  // 9100) but in a clearer way. Use the infix binary operator composeOptional
  // to compose a lense with an optional, and use 'andThen' to compose
  // the optional with a lense).

  //  (1-2 lines)

  lazy val itu2: University = ???

  // There is a test in LensesSpec to test whether what you have built behaves
  // as expected.
  //
  // Now once you provide lenses for your types, navigating and modifying deep
  // structures becomes more readable and easier to write.  In fact, lense
  // libraries provide various mechanisms to generate them for the properties of
  // your case classess, so this access can come at almost no (coding) cost.

  // Exercise 8

  // Monocle provides compiler macros (a part of Scala we side step in ADPRO),
  // which allow very concise generation of lenses for case classes. For
  // instance _zipcode from above can be easily generated as follows:

  val _zipcode1: Lens[Address, ZipCode] = Focus[Address] (_.zipcode)

  // Define _students1 analogously, complete itu3 to use these new lenses (with
  // the same specification as itu2).

  lazy val _students1: Lens[University, Students] = ???

  lazy val itu3: University = ???

  // As a curiosity, here is an example that this can be moved even closer to
  // imperative style, using extension methods and macros:

  val itu4: University =
    itu
      .focus (_.students)
      .at ("Alex")
      .some
      .andThen (_zipcode) // focus syntax is not implemented for Optionals in monocle
      .replace ("9100")

  // Note how similarly it would look in Java, imperative style
  //
  // try {
  //    itu
  //      .getStudents ()
  //      .get ("Alex")
  //      .setZipcode ("9100")
  // } catch NoSuchElementException { }

  // Exercise 9
  //
  // Turn names of all countries in all the addresses of all students in the itu
  // object into uppercase. Using lenses of course.
  //
  // We shall use the 'modify' function of lenses. Morris describes modify
  // problem in Section 2, and shows the lens solution in Listing 9.  Monocle
  // has a modify method in Lens[A,B]:
  //
  //    modify : (B => B) => A => A
  //
  // It works like a simulatnous combination of get and replace at the same time.
  // We use modify if we need to get a value, and then make a modification to
  // it.  Modify takes a function that makes the change (computes the new data)
  // and then the source (concrete) object.  It returns the new object. It is
  // potentially more efficient than using get and replace separately.  So modify is
  // "map" that applies at some nested level in a complex structure.
  //
  // In this exercise, we use modify to perform a cross cutting modification
  // on a university object.
  //
  // We will need a lense that gives us all countries from the map of students.
  // This kind of lense is called a Traversable in Monocle.
  //
  // We use 'andThen' to compose an optical (Lens, Traversable,
  // Optional, etc) with a traversable (as we used 'andThen' above to
  // compose any of these with a Lens).
  //
  // The traversable "each" (which has a default instance for maps) will give us
  // a collection of all objects (values in a map).  So to solve the task we
  // need to compose:
  //
  // - a lense that extracts the students collection from a University
  // (_students)
  //
  // - a traversable that extracts all objects from a collection (each)
  //
  // - a lense that extract the country from an address object (_country, you
  // will need to write that one, as we did not create it yet). Let's start with
  // _country (1 line):

  lazy val _country: Lens[Address,String] = ???

  // Now compute itu5, that has the same entries as ITU, but all country names
  // are upper case. (1-6 lines)

  lazy val itu5: University = ???

  // LensesSpec.scala has a test to see if you succeeded.

  // QUESTION: Compare the test with the code used above.  Why have we used
  // lenses/traversals above, and not in the test? What is the difference
  // between the code in the test and the code above that influences this? Write
  // the answer below.  Explain below.
  //
  // ...

  // Exercise 10
  //
  // Use filterIndex (p) to only capitalize city names of the students on the
  // list whose name satisfies predicate 'p'. Let's capitalize the names of
  // students whose name begins with letter A.  The filterIndex combinator is a
  // traversal, like 'each' above. Recall that 'andThen' is used to
  // compose (append) a traversal or a lense.

  lazy val itu6: University = ???

  // Exercise 11
  //
  // We are returning to the construction of basic lenses.  Implement a
  // (partial) lens that accesses the i-th element of a list (let's call it
  // 'ith').  A partial lens, so an Optional in Monocle terminology, would be
  // of type Optional[List[A],A].  The Optional takes two parameters for the
  // constructor:
  //
  // get: List[A] => Option[A] replace: A => List[A] => List[A]

  // You will need to decide what to do with the setter if n is greater than the
  // length of the list.  One option is to do nothing, just ignore the setting.
  // Another alternative is to provide a default element, and extend the list
  // approprietly if accessed or set beyond the size. In the latter case we
  // obtain a total lense.  We first try the partial version (ca. 5-8 lines):

  def ith[A] (n: Integer): Optional[List[A], A] = ???

  // Now create the total lense with a default value that extends the
  // list, if the list is too short (8-15 lines):

  def ith1[A] (default: A) (n: Integer): Lens[List[A], A] = ???

  // Exercise 12
  //
  // The lens 'ith' demonstrates that lenses emulate a form of "imperative"
  // programming, by making a structure updatedable, even deeply.  Use 'ith'
  // to increment the third element on a list 'list0'

   val list0: List[Int] = List (1,2,3,4,5,6)

   // list1 should contain list0 with the third element incremented:

   lazy val list1: List[Int] = ???

}
