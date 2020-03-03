package Chapter2

object Chapter2 extends App {

  def abs(n: Int): Int =
    if (n < 0) -n else n

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  private def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d."
    msg.format(n, factorial(n))
  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  println(formatAbs(-42))
  println(formatFactorial(7))

  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    def loop(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n + 1)

    loop(0)
  }

  println("EXERCISE")

  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc else go(n - 1, n * acc)

    go(n, 1)
  }

  //exercise 2.1
  def fib(n: Int): Int = {
    def go(one: Int, two: Int, count: Int): Int =
      if (count == 0) one + two else go(two, one + two, count - 1)

    if (n <= 0) 0
    else if (n == 1 || n == 2) 1
    else go(1, 1, n - 3)
  }

  //  println(fib(4))

  // 無限ストリームで作ってみた
  def fibfib(n: Int): Int = {
    def go(a: Int = 0, b: Int = 1): Stream[Int] = Stream.cons(a, go(b, a + b))

    go().take(n + 1).reverse.head
  }

  //  println(fibfib(4))

  // exercise 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean =
    as.zip(as.tail).map(a => ordered(a._1, a._2)).reduce(_ && _)

  // exercise 2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => ((b: B) => f(a, b))

  // exercise 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  // exercise 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))
}
