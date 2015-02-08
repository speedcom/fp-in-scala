def rollDie: Int = {
  val rng = new scala.util.Random
  rng.nextInt(6)
}

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {

  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)

    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

// hypothetical code
class Foo {
  private var s: FooState = ???
  def bar: Bar = ???
  def baz: Int = ???
}


def randomPair(rng: RNG): (Int, Int) = {
  val (a, rng2) = rng.nextInt
  val (b, _) = rng2.nextInt
  (a, b)
}

// 6.1
def nonNegativeInt(rng: RNG): (Int, RNG) = {
  val (i, r) = rng.nextInt
  (if (i < 0) -(i + 1) else i, r)
}

// 6.2
def double(rng: RNG): (Double, RNG) = {
  val (i, r) = nonNegativeInt(rng)
  (i / (Int.MaxValue.toDouble + 1), r)
}

// 6.3
def intDouble(rng: RNG): ((Int,Double), RNG) = {
  val (a, rng2) = rng.nextInt
  val (b, rng3) = rng2.nextInt
  ((a, b.toDouble), rng3)
}
def doubleInt(rng: RNG): ((Double,Int), RNG) = {
  val ((i, d), rng2) = intDouble(rng)
  ((d, i), rng2)
}
def double3(rng: RNG): ((Double,Double,Double), RNG) = {
  val (a, rng2) = rng.nextInt
  val (b, rng3) = rng2.nextInt
  val (c, rng4) = rng3.nextInt
  ((a.toDouble, b.toDouble, c.toDouble), rng4)
}

// 6.4
def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
  if(count == 0)
    (Nil, rng)
  else {
    val (a, rng2) = rng.nextInt
    val (xs, rng3) = ints(count-1)(rng2)
    (a :: xs, rng3  )
  }
}

type Rand[+A] = RNG => (A, RNG)

val int: Rand[Int] = _.nextInt

def unit[A](a: A): Rand[A] = rng => (a, rng)

def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
  val (a, rng2) = s(rng)
  (f(a), rng2)
}

// 6.5 - TODO

// 6.6
def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
  val (a, rng2) = ra(rng)
  val (b, rng3) = rb(rng2)
  (f(a,b), rng3)
}

def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] = map2(ra,rb)((_,_))

val randIntDouble: Rand[(Int, Double)] = both(int, double)
val randDoubleInt: Rand[(Int, Double)] = both(double, int)

// 6.7
def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))











