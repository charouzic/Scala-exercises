// Andrzej Wąsowski, IT University of Copenhagen

object MyModule {

  def abs(n: Int): Int = if (n < 0) -n else n

  /* Exercise 1 */
  def square (n: Int): Int = n*n

  private def formatAbs(x: Int) =
    s"The absolute value of $x is ${abs (x)}"

  val magic :Int = 42
  var result :Option[Int] = None

  def main(args: Array[String]): Unit = {
    assert (magic - 84 == magic.-(84))
    println (formatAbs (magic-100))

    def f(n: Int): Int = {
      // print  even numbers
      def loop(n : Int): Unit =
        if (n == 2) println(2)
        else {
          println(n)
          loop(n-2)
        }

      loop(n*2)

      n
    }

    println(f(10))
  }
}
