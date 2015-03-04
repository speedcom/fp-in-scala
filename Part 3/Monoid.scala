trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

// exemplary instnaces
val stringMonoid = new Monoid[String] {
  def op(s1: String, s2: String) = s1 + s2
  def zero = ""
}

val intMonoid = new Monoid[Int] {
  def op(i1: Int, i2: Int) = i1 + i2
  def zero = 0
}

def listMonoid[A] = new Monoid[List[A]] {
  def op(l1: List[A], l2: List[A]) = l1 ++ l2
  def zero = Nil
}