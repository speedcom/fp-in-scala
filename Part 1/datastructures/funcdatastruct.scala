sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](x: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  EX 3.2
  def removeFirst[A](as: List[A]): List[A] =
    if (as.isEmpty) Nil
    else as.tail

  // EX 3.3
  def setHead[A](v: A, as: List[A]): List[A] =
    if (as.isEmpty) Cons(v, Nil)
    else v :: as.tail

  // EX 3.4
  def drop[A](l: List[A], n: Int): List[A] = {
    if(n > 0 && l.length > 0) {
      drop(l.tail, n-1)
    } else {
      l
    }
  }

  def dropWhile[A](as: List[A])(f: A => Boolean) = as match {
    case Cons(h, t) if f(h) => dropWhile(t)(f)
    case _ => as
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(x, xs) => Cons(x, append(xs, a2))
  }

  def init[A](l: List[A]): List[A] = {
    def go[A](l: List[A], result: List[A] = Nil): List[A] = l match {
      case Nil => result
      case Cons(x, Nil) => Cons(x, go(Nil, result))
      case Cons(x, xs) => Cons(x, go(xs, result))
    }
    go(l)
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def length[A](as: List[A]): Int = foldRight(as, 0)((_, z) => z+1)

  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(foldLeft(xs, z)(f), x)
  }
}

















