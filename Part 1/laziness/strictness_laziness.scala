sealed trait Stream[+A] {

  def toList: List[A] = {
    @annotation.tailrec
    def toList_[A](st: Stream[A], xs: List[A]): List[A] = st match {
      case Empty => xs
      case Cons(h,t) => toList_(t(), h()::xs)
    }
    toList_(this, List()).reverse
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n >  1 => Cons(h(), t().take(n-1))
    case Cons(h, _) if n == 0 => Cons(h(), empty)
    case _ => empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h,t) => p(h()) || t() exists p
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h,t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists2(p: A => Boolean): Boolean = foldRight(false)((a,b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) && t().forAll(p)
    case Cons(h, _) => p(h())
    case _ => false
  }

  def forAll2(p: A => Boolean): Boolean = foldRight(false)((a,b) => p(a) && b)

  def takeWhile2(p: A => Boolean): Stream[A] = foldRight(empty[B])((a,b) => if(p(a)) Cons(a, b) else empty)

  def headOption: Option[A] = foldRight(None: Option[A])((h,_) => Some(h))

  def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a,b) => Cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] = foldRight(empty[A])((a, b) => if(f(a)) Cons(a, b) else b)

  def append[B >: A](z: => Stream[B]): Stream[B] = foldRight(z)((a, b) => Cons(a,b))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((a, b) => f(a) append b)

  def constant[A](a: A): Stream[A] = Cons(a, constant(a))

  def from(n: Int): Stream[Int] = Cons(n, from(n+1))

  def fibs: Stream[Int] = foldRight(0)((a,b) => a+b)

}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](h: => A, t: => Stream[A]) = {
    lazy val head = h
    lazy val tail = t
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if(as.isEmpty) Empty else cons(as.head, apply(as.tail: _*))

  def unfold[A, S](z: S)(f: S => Option[(A,S)]): Stream[A] = f(z) match {
    case None => empty
    case Some((h,s)) => cons(h, unfold(s)(f))
  }

  def fibsViaUnfold: Stream[Int] = unfold((0,1))(case (f0, f1) => Some((f0, (f1, f0+f1)))

  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(s => Some((s, s+1)))

  def constantViaUnfold[A](a: A): Stream[A] = unfold(a)(_ => Some((a,a)))

  def onesViaUnfold[A](a: A): Stream[A] = unfold(1)(_ => Some((1,1)))

}