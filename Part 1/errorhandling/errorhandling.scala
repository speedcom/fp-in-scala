sealed trait Option[+A]
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

import scala.{Option => _, Either => _, _}

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this.flatMap(x => x.orElse(None))

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(x) => f(x)
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(x) => x
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = ???

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(x) if f(x) => this
    case None => None
  }
}

// ex 4.3
def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = (a, b) match {
  case (None, _) | (_, None) => None
  case (Some(x1), Some(x2)) => Some(f(x1,x2))
}

// ex 4.4 -- TODO
def sequence[A](a: List[Option[A]]): Option[List[A]] = {
  a.find(_ == None).flatten.map(x => a.map(_.get))
}

// ex 4.5 -- TODO
def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = {

  def go[A,B](aa: List[B], rest: List[A]): Option[List[B]] = rest match {
    case h :: t => f(h) match {
      case Some(x) => go(x :: aa, t)
      case _ => None
    }
  }

  go(Nil, a)

}














