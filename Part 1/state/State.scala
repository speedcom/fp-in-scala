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

def nonNegativeInt(rng: RNG): (Int, RNG) = {
  val (a, rng2) = rng.nextInt
  if(a >= 0 && a < Int.MaxValue) (a, rng2)
  else nonNegativeInt(rng2)
}

def double(rng: RNG): (Double, RNG) = {
  val (a, rng2) = nonNegativeInt(rng)
  if(a >= 0 && a < 1) (a.toDouble, rng2)
  else double(rng2)
}