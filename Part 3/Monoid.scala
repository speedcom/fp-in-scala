trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

// exemplary instnaces
val stringMonoid = new Monoid[String] {
  def op(s1: String, s2: String) = s1 + s2
  def zero = ""
}

def listMonoid[A] = new Monoid[List[A]] {
  def op(l1: List[A], l2: List[A]) = l1 ++ l2
  def zero = Nil
}

// EX 10.1
val intAddtion = new Monoid[Int] {
  def op(i1: Int, i2: Int) = i1 + i2
  def zero = 0
}

val intMultiplication = new Monoid[Int] {
  def op(i1: Int, i2: Int) = i1 * i2
  def zero = 1
}

val booleanOr: Monoid[Boolean] {
  def op(i1: Boolean, i2: Boolean) = i1 || i2
  def zero = false
}

val booleanOr: Monoid[Boolean] {
  def op(i1: Boolean, i2: Boolean) = i1 && i2
  def zero = true
}
