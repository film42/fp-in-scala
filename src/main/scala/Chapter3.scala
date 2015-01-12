/**
 * Created by: film42 on: 1/11/15.
 */

object GivenCode {
  sealed trait List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x,xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x,xs) => x * product(xs)
    }

    def tail[A](list: List[A]): List[A] = list match {
      case Nil => Nil
      case Cons(x, xs) => xs
    }

    @annotation.tailrec
    def drop[A](list: List[A], n: Int): List[A] = {
      if(n <= 0) list
      else drop(tail(list), n - 1)
    }

    @annotation.tailrec
    def dropWhile[A](list: List[A])(predicate: A => Boolean): List[A] = list match {
      case Nil => Nil
      case Cons(x, xs) => if(predicate(x)) dropWhile(xs)(predicate) else list
    }

    def setHead[A](list: List[A], head: A): List[A] = list match {
      case Nil => Cons(head, Nil)
      case Cons(x, xs) => Cons(head, xs)
    }

    def init[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
    }

    def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B =
      l match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }

    def product2(l: List[Double]): Double =
      foldRight(l, 0.0)(_ * _)

    def length[A](l: List[A]): Int =
      foldRight(l, 0)((_, acc) => acc + 1)

    @annotation.tailrec
    def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

    def sum2(ints: List[Int]): Int =
      foldLeft(ints, 0)(_ + _)

    def product3(ds: List[Double]): Double =
      foldLeft(ds, 1.0)(_ * _)

    def reverse[A](l: List[A]): List[A] =
      foldLeft(l, List[A]())((b, a) => Cons(a, b))

    def foldRight2[A,B](l: List[A], z: B)(f: (B, A) => B): B =
      foldLeft(reverse(l), z)(f)

    def foldLeft2[A,B](l: List[A], z: B)(f: (B, A) => B): B =
      // Not sure about this one.. need to review this more.
      foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

    def append[A](a1: List[A], a2: List[A]): List[A] =
      {} //foldLeft(a1, Nil)(Cons(_, _))

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }
}

object Chapter3 extends App {
  import GivenCode._

  // Exercise 1
  val list_ex1: List[Int] = List()
  val result = list_ex1 match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + List.sum(t)
    case _ => 101
  }
  // println(result)

  // Exercise 2: Implement tail
  val list_1_ex2 = List(1, 2, 3, 4)
  // println(List.tail(list_1_ex2))
  val list_2_ex2 = List(1)
  // println(List.tail(list_2_ex2))

  // Exercise 3: Implement drop
  val list_1_ex3 = List(1, 2, 3, 4)
  // println(List.drop(list_1_ex3, 1))
  // println(List.drop(list_1_ex3, 3))
  // println(List.drop(list_1_ex3, 100))

  // Exercise 4: Implement dropWhile
  val list_1_ex4 = List(1, 2, 3, 4)
  // println(List.dropWhile(list_1_ex4)(_ < 4))

  // Exercise 5: Implement setHead
  val list_1_ex5 = List(1, 2, 3, 4)
  // println(List.setHead(list_1_ex5, 20))

  // Exercise 6: Implement init
  val list_1_ex6 = List(1, 2, 3, 4)
  // println(List.init(list_1_ex6))

  // Exercise 7: Implement short circuit product2
  // I'm not sure this can be done without checking the list before hand,
  // which means just run through the whole list.

  // Exercise 8
  // println(List.foldRight(List(1, 2, 3, 4), Nil: List[Int])(Cons(_, _)))

  // Exercise 9: Implement length
  val list_1_ex9 = List(1, 2, 3, 4)
  // println(List.length(list_1_ex9))

  // Exercise 10: Implement foldLeft
  // println(List.foldLeft(list_1_ex9, 0)(_ + _))

  // Exercise 11: Implement bunch of stuff

  // Exercise 12: Implement reverse
  println(List.reverse(list_1_ex9))

}
