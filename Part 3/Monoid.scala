trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

// exemplary instances
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

val booleanOr = new Monoid[Boolean] {
  def op(i1: Boolean, i2: Boolean) = i1 || i2
  def zero = false
}

val booleanAnd = new Monoid[Boolean] {
  def op(i1: Boolean, i2: Boolean) = i1 && i2
  def zero = true
}

// EX 10.2
def optionMonoid[A] = new Monoid[Option[A]] = {
  def op(op1: Option[A], op2: Option[A]) = op1 orElse op2
  def zero = None
}

// EX 10.3
// A function having the same argument and return type is sometimes called an endofunction.
// This is a monoid for endofunctions.
def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
  def op(e1: A => A, e2: A => A): A => A = a => e1 compose e2
  def zero: A => A = a => a
}

def concatenate[A](l: List[A])(m: Monoid[A]) = l.foldLeft(m.zero)(m.op)

// EX 10.5
def foldMap[A,B](as: Seq[A], m: Monoid[B])(f: A => B): B = as.foldLeft(m.zero)((a, b) => m.op(a, f(b)))

// EX 10.7
def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
  val l = v.length
  if(l > 1) {
    val (is1, is2) = v.splitAt(l/2)
    m.op(foldMapV(is1, m)(f), foldMapV(is2, m)(f))
  } else {
    foldMap(v, m)(f)
  }
}

// EX 10.9
def orderingIntMonoid(is: IndexedSeq[Int]) = ???

sealed trait WC
case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC

// EX 10.10
val wcMonoid: Monoid[WC] = new Monoid[WC] {
  def op(wc1: WC, wc2: WC): WC = (wc1, wc2) match {
    case (Stub(ch1), Stub(ch2))       => Stub(ch1+ch2)
    case (Stub(ch1), Part(ls, w, rs)) => Part(ch1 + ls, w, rs)
    case (Part(ls, w, rs), Stub(ch2)) => Part(ls, w, rs+ch2)
    case (Part(l1, w1, r1), Part(l2, w2, r2)) => Part(l1, w1 + (if ((r1 + l2).isEmpty) 0 else 1) + w2, r2)
  }

  def zero: WC = Stub("")
}

















