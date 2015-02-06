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
  val (a, rng2) = rng.nextInt
  if(a >= 0 && a < Int.MaxValue) (a, rng2)
  else nonNegativeInt(rng2)
}

// 6.2
def double(rng: RNG): (Double, RNG) = {
  val (a, rng2) = nonNegativeInt(rng)
  if(a >= 0 && a < 1) (a.toDouble, rng2)
  else double(rng2)
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
  val (b, rng2) = rng.nextInt
  val (c, rng2) = rng.nextInt
  (a.toDouble, b.toDouble, c.toDouble)
}