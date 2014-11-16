package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  } 
  
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }
  
  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42 
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101 
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

//  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
//    as match {
//      case Nil => z
//      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
//    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(as, z)((b,a) => f(a,b))
  
  def sum2(ns: List[Int]) = 
    foldRight(ns, 0)((x,y) => x + y)
  
  def product2(ns: List[Double]) = 
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(x, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => if(n<=0){l}else{drop(xs, n-1)}
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(head, tail) if(f(head)) => dropWhile(tail, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = sys.error("todo")

  def length[A](l: List[A]): Int = foldRight(l, 0)((x, y) => 1 + y)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
  }

//  def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B = foldRight(l, z)((a, b) => f(b, a))

  def sum3(ns: List[Int]) = foldLeft(ns, 0)(_ + _)
  def product3(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)
  def length3(ns: List[Int]) = foldLeft(ns, 0)((b, a) => 1 + b)

  def reverse[A](xs : List[A]) : List[A] = foldLeft(xs, Nil : List[A]){ (acc, elem) => Cons(elem, acc) }

  def append2[A](l : List[A], x : List[A]) : List[A] = foldLeft(reverse(l), x){ (acc, elem) => Cons(elem, acc) }

  def concat[A](ll : List[List[A]]) : List[A] = foldLeft(reverse(ll), Nil: List[A]){(acc, l) => append2(l, acc)}

  def incr(xs : List[Int]) : List[Int] = foldLeft(reverse(xs), Nil : List[Int]){(acc, elem) => Cons(elem+1, acc)}

  def dblToStr(xs : List[Double]) : List[String] = foldLeft(reverse(xs), Nil : List[String]){(acc, elem) => Cons(elem.toString, acc)}

  def map[A,B](l: List[A])(f: A => B): List[B] = foldLeft(reverse(l), Nil : List[B]){(acc, elem) => Cons(f(elem), acc)}

  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldLeft(reverse(as), Nil: List[A]){(acc, elem) => if(f(elem)){Cons(elem, acc)}else{acc}}

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = concat(map(as)(f))

  def filter2[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as){a => if(f(a)){List(a)}else{Nil}}

  def aggr(xs : List[Int], ys : List[Int]) : List[Int] = (xs, ys) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(xHead, xTail), Cons(yHead, yTail)) => Cons(xHead + yHead, aggr(xTail, yTail))
  }

  def zipWith[A](xs : List[A], ys : List[A])(f : (A,A) => A) : List[A] = (xs, ys) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(xHead, xTail), Cons(yHead, yTail)) => Cons(f(xHead,yHead), zipWith(xTail, yTail)(f))
  }
  

}