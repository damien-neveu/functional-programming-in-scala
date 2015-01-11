package fpinscala.laziness

import Stream._

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def scanRight[B](z: => B)(f: (Stream[A], => B) => B): Stream[B] = this match {
    case Empty => Stream[B](z)
    case Cons(h, t) => Stream.cons(f(this, z), t().scanRight(z)(f))
  }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList : List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h()::t().toList
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if (n>0) => Cons(h, () => t().take(n-1))
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n>0 => t().drop(n-1)
    case s: Stream[A] => s
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
    case s: Stream[A] => Empty
  }

  def takeWhile2(p: A => Boolean): Stream[A] = foldRight(Empty : Stream[A]){ (a, b) => if(p(a)){Stream.cons(a, b)}else{Stream.empty} }

  def forAll(p: A => Boolean): Boolean = foldRight(true){(a, b) => p(a) && b}

  def map[B](f : A => B): Stream[B] = foldRight(Empty: Stream[B]){ (a, b) => Stream.cons(f(a), b) }

  def filter(p : A => Boolean): Stream[A] = foldRight(Empty: Stream[A])( (h, t) => if(p(h)){Stream.cons(h, t)}else{t} )

  def append[B>:A](s2: => Stream[B]): Stream[B] = foldRight(s2)( (h, t) => Stream.cons(h, t) )

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(Empty: Stream[B])( (h, t) => f(h) append t )

  def headOption : Option[A] = foldRight(None: Option[A])( (h, t) => Some(h) )

  def startsWith[B](s: Stream[B]): Boolean = this zipAll(s) filter(!_._2.isEmpty) forAll( el => el._1 == el._2 )

  def tails: Stream[Stream[A]] = unfold(this) {
    case Empty => None
    case strm => Some(strm, strm.drop(1))
  } append Stream(Empty)

  def tails2: Stream[Stream[A]] = scanRight(Empty : Stream[A]){ (strm, z) => strm append z }

  def map2[B](f : A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some( (f(h()), t()) )
    case Empty => None
  }
  def take2(n: Int): Stream[A] = unfold((this, n)) {
    case (Cons(h, t), x) => if(x>0){Some( h(), (t(), x-1))}else{None}
    case _ => None
  }
  def takeWhile3(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if(p(h())) => Some(h(), t())
    case _ => None
  }
  def zipWith[B,C](strm: Stream[B])(f: (A,B) => C): Stream[C] = unfold((this, strm)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some( (f(h1(), h2()), (t1(), t2())) )
    case _ => None
  }
  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some( (Some(h1()), Some(h2())), (t1(), t2()) )
    case (Cons(h1, t1), Empty) => Some( (Some(h1()), None : Option[B]), (t1(), Empty) )
    case (Empty, Cons(h2, t2)) => Some( (None : Option[A], Some(h2()) : Option[B]), (Empty, t2()) )
    case (Empty, Empty) => None
  }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n+1))

  def constant[A](a : A) : Stream[A] = Stream.cons(a, constant(a))

  def fibs : Stream[Int] = {
    def internalFibs(n_2: Int, n_1 : Int) : Stream[Int] = Stream.cons(n_2+n_1, internalFibs(n_1, n_2+n_1))
    Stream.cons(0, Stream.cons(1, internalFibs(0, 1)))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => Empty
    case Some( (a, s) ) => Stream.cons(a, unfold(s)(f))
  }

  def from2(n : Int) : Stream[Int] = unfold(n)( s => Some(n, n+1) )
  def constant2[A](a : A) : Stream[A] = unfold(a)( s => Some(a, a) )
  def fibs2 : Stream[Int] = Stream.cons(0, Stream.cons(1, unfold( (0,1) )( s => Some(s._1 + s._2, (s._2, s._1+s._2)) ) ))


}