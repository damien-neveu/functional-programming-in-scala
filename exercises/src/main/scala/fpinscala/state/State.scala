package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

//  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
//    rng => {
//      val (a, rng2) = s(rng)
//      (f(a), rng2)
//    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (nnInt, rng2) = rng.nextInt
    val nnInt2 = if(nnInt==Int.MinValue){
      Int.MaxValue
    }
    else if(nnInt<0){
      -nnInt
    }
    else{
      nnInt
    }
    (nnInt2, rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (nnInt, rng2) = nonNegativeInt(rng)
    val dd = if (nnInt==Int.MaxValue) {
      (Int.MaxValue-1).toDouble / Int.MaxValue
    }
    else {
      nnInt.toDouble / Int.MaxValue
    }
    (dd, rng2)
  }

  def double2(rng: RNG): (Double, RNG) = {
    map[Int, Double](nonNegativeInt)(i => if(i==Int.MaxValue){(Int.MaxValue-1).toDouble/Int.MaxValue}else{i.toDouble/Int.MaxValue})(rng)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (nnInt, rng2) = nonNegativeInt(rng)
    val (dd, rng3) = double(rng2)
    ((nnInt, dd), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((nnInt, dd), newRng) = intDouble(rng)
    ((dd, nnInt), newRng)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (dd1, rng2) = double(rng)
    val (dd2, rng3) = double(rng2)
    val (dd3, rng4) = double(rng3)
    ((dd1, dd2, dd3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def internal(the_count : Int, the_rng : RNG, the_ints : List[Int]) : (List[Int], RNG) = if(the_count<=0){
      (the_ints, the_rng)
    }
    else {
      val (nnInt, newRng) = nonNegativeInt(the_rng)
      internal(the_count-1, newRng, nnInt :: the_ints)
    }
    internal(count, rng, List())
  }

//  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
//    val (resA, newRng01) = ra(rng)
//    val (resB, newRng02) = rb(newRng01)
//    (f(resA, resB), newRng02)
//  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    def internal(the_rands : List[Rand[A]], the_as: List[A], the_latest_rng: RNG): (List[A],RNG) = the_rands match {
      case Nil => (the_as, the_latest_rng)
      case head::tail => {
        val (a_new_a, a_new_rng) = head(the_latest_rng)
        internal(tail, a_new_a :: the_as, a_new_rng)
      }
    }
    rng => internal(fs, List(), rng)
  }

  def ints2(count: Int)(rng: RNG): (List[Int], RNG) = sequence(List.fill(count)(RNG.int))(rng)

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s){ a =>
    //rng => (f(a), rng)
    unit(f(a))
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra) { a =>
    map(rb){ b => f(a, b) }
  }

  def nonNegativeLessThan(n : Int) : Rand[Int] =
    flatMap(nonNegativeInt){ i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) {
        //rng => (mod, rng)
        unit(mod)
      }
      else {
        nonNegativeLessThan(n)
      }
    }

}

//def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
//  def internal(the_rands : List[Rand[A]], the_as: List[A], the_latest_rng: RNG): (List[A],RNG) = the_rands match {
//    case Nil => (the_as, the_latest_rng)
//    case head::tail => {
//      val (a_new_a, a_new_rng) = head(the_latest_rng)
//      internal(tail, a_new_a :: the_as, a_new_rng)
//    }
//  }
//  rng => internal(fs, List(), rng)
//}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = flatMap { a => State.unit(f(a)) }
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap { a => sb.map( b => f(a, b)) }
  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    State(s => {
        val (a01, s01) = run(s)
        f(a01).run(s01)
      }
    )
  }
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    def internal(the_sas: List[State[S, A]], the_as : List[A], the_s : S) : (List[A],S) = the_sas match {
      case Nil => (the_as.reverse, the_s)
      case head::tail => {
        val (a01, s01) = head.run(the_s)
        internal(tail, a01 :: the_as, s01)
      }
    }
    State( s => internal(sas, List(), s))
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    def internal(the_inputs : List[Input], the_machine : Machine) : ((Int, Int), Machine) = the_inputs match {
      case Nil => ((the_machine.coins, the_machine.candies), the_machine)
      case head::tail => head match {
        case Coin => if (the_machine.locked && the_machine.candies>0){
          internal(tail, the_machine.copy(locked=false,coins=the_machine.coins+1))
        } else {
          internal(tail, the_machine.copy(coins=the_machine.coins+1))
        }
        case Turn => if (!the_machine.locked && the_machine.candies>0) {
          internal(tail, the_machine.copy(locked=true,candies=the_machine.candies-1))
        } else {
          internal(tail, the_machine)
        }
      }
    }
    val f =  {
      m: Machine => internal(inputs, m)
    }
    State(f)
  }
}
