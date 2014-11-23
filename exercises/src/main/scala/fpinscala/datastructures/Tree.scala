package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  // usage : Tree.size( Branch( Branch( Branch(Leaf(7),Leaf(1)) , Branch(Leaf(5),Leaf(10)) ), Branch(Branch(Branch(Leaf(9),Leaf(12)), Branch(Leaf(15),Leaf(3))) , Branch(Leaf(1), Leaf(11))) ) )
  def size[A](tree : Tree[A]) : Int = tree match {
    case Leaf(v) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def size2[A](tree : Tree[A]) : Int = fold(tree)(a => 1)((l,r) => 1 + l + r)

  def max(tree : Tree[Int]) : Int = tree match {
    case Leaf(v) => v
    case Branch(l, r) => max(l) max max(r)
  }

  def max2(tree : Tree[Int]) : Int = fold(tree)(a => a)((l,r) => l max r)

  def depth[A](tree : Tree[A]) : Int = tree match {
    case Leaf(v) => 1
    case Branch(l, r) => 1+depth(l) max 1+depth(r)
  }

  def depth2[A](tree : Tree[A]) : Int = fold(tree)(a => 1)((l,r) => (1+l) max (1+r))

  def map[A,B](tree : Tree[A])(f : A => B) : Tree[B] = tree match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def map2[A,B](tree : Tree[A])(f : A => B) : Tree[B] = fold(tree)(a => Leaf(f(a)) : Tree[B])((l, r) => Branch(l, r))

  def fold[A,B](tree : Tree[A])(leafAction : A => B)(branchAction : (B, B) => B) : B = tree match {
    case Leaf(v) => leafAction(v)
    case Branch(l, r) => branchAction(fold(l)(leafAction)(branchAction), fold(r)(leafAction)(branchAction))
  }

}