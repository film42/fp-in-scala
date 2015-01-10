/**
 * Created by: film42 on: 1/9/15.
 */
object Chapter2 extends App {

  // Exercise 1
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(x: Int, y: Int, steps: Int): Int = {
      if(steps <= 0) y
      else go(y, x + y, steps - 1)
    }

    go(0, 1, n - 2)
  }

  //  println(fib(10000))

  // Exercise 2
  def isSorted[A](as: Array[A], gt: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(first: A, second: A, index: Int): Boolean = {
      if(index >= as.length) true
      else {
        if(gt(first, second)) false
        else go(as(index), as(index + 1), index + 1)
      }
    }

    if(as.length < 2) true
    else go(as(0), as(1), 1)
  }

  //  println(isSorted(Array(1d,2d,3d,6d,5d), (x: Double, y: Double) => x > y))

  // Exercise 3
  def partial1[A,B,C](a: A, f: (A,B) => C): B => C =
    (x: B) => f(a, x)

  val divisibleByTen = partial1(10, (x: Int, y: Int) => y % x == 0)
  // println(divisibleByTen(20))
  // println(divisibleByTen(2))

  // Exercise 4
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    a => b => f(a, b)
  }

  val curriedAdd = curry((x: Int, y: Int) => x + y)
  // println(curriedAdd(3)(5) == 8)

  // Exercise 5
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  // println(uncurry(curriedAdd)(5, 3) == 8)

  // Exercise 6
  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    a: A => f(g(a))
  }

  val add5 = (x: Int) => x + 5
  val multiply3 = (x: Int) => x * 3
  val add5ThenMultiply3 = compose(multiply3, add5)
  // println(add5ThenMultiply3(5) == 30)
}
