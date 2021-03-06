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
val intAddition = new Monoid[Int] {
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

trait Foldable[F[_]] {
  def foldRight[A,B](as: F[A])(z: B)(f: (A,B) => B): B
  def foldLeft[A,B](as: F[A])(z: B)(f: (B,A) => B): B
  def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B
  def concatenate[A](as: F[A])(m: Monoid[A]): A = foldLeft(as)(m.zero)(m.op)
  def toList[A](fa: F[A]): List[A] = foldLeft(fa)(List[A]())((l, a) => a :: l)
}

// EX 10.12
object ListFoldable extends Foldable[List] {
  def foldRight[A,B](as: List[A])(z: B)(f: (A,B) => B): B = as.foldRight(z)(f)
  def foldLeft[A,B](as: List[A])(z: B)(f: (B,A) => B): B = as.foldLeft(z)(f)
  def foldMap[A,B](as: List[A])(f: A => B)(mb: Monoid[B]): B = as.foldLeft(mb.zero)((b, a) => mb.op(b, f(a)))
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  def foldRight[A,B](as: IndexedSeq[A])(z: B)(f: (A,B) => B): B = as.foldRight(z)(f)
  def foldLeft[A,B](as: IndexedSeq[A])(z: B)(f: (B,A) => B): B = as.foldLeft(z)(f)
  def foldMap[A,B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B = foldMapV(as, mb)(f)
}

object StreamFoldable extends Foldable[Stream] {
  def foldRight[A,B](as: Stream[A])(z: B)(f: (A,B) => B): B = as.foldRight(z)(f)
  def foldLeft[A,B](as: Stream[A])(z: B)(f: (B,A) => B): B = as.foldLeft(z)(f)
  def foldMap[A,B](as: Stream[A])(f: A => B)(mb: Monoid[B]): B = ???
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

// EX 10.13
object TreeFoldable extends Foldable[Tree] {
  def foldRight[A,B](as: Tree[A])(z: B)(f: (A,B) => B): B = as match {
    case Leaf(v) => f(v, z)
    case Branch(lt, rt) => foldRight(lt)(foldRight(rt)(z)(f))(f)
  }
  def foldLeft[A,B](as: Tree[A])(z: B)(f: (B,A) => B): B = as match {
    case Leaf(v) => f(z, v)
    case Branch(lt, rt) => foldLeft(rt)(foldLeft(lt)(z)(f))(f)
  }
  def foldMap[A,B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B = as match {
    case Leaf(v) => f(v)
    case Branch(lt, rt) => mb.op(foldMap(lt)(f)(mb), foldMap(rt)(f)(mb))
  }
}

// EX 10.14
object OptionFoldable extends Foldable[Option] {
  def foldRight[A,B](as: Option[A])(z: B)(f: (A,B) => B): B = as.map(a => f(a, z)).getOrElse(z)
  def foldLeft[A,B](as: Option[A])(z: B)(f: (B,A) => B): B = as.map(a => (f, z)).getOrElse(z)
  def foldMap[A,B](as: Option[A])(f: A => B)(mb: Monoid[B]): B = as.map(f).getOrElse(mb.zero)
}

// EX 10.16
def productMonoid[A,B](ma: Monoid[A], mb: Monoid[B]): Monoid[(A,B)] = new Monoid[(A,B)] {
  def zero: (A,B) = (ma.zero, mb.zero)
  def op(m1: (A,B), m2: (A,B)): (A,B) = (ma.op(m1._1, m2._1), mb.op(m1._2, m2._2))
}

def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
  new Monoid[Map[K, V]] {
    def zero = Map[K,V]()
    def op(a: Map[K, V], b: Map[K, V]) = (a.keySet ++ b.keySet).foldLeft(zero) { (acc,k) =>
      acc.updated(k, V.op(a.getOrElse(k, V.zero), b.getOrElse(k, V.zero)))
    }
}



