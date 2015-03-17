trait Applicative[F[_]] extends Functor[F] {
  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B]
  def unit[A](a: => A): F[A]

  def map[A,B](fa: F[A])(f: A => B): F[B] = apply(unit(f))(fa)
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] = ???

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(fa => fa)

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] = sequence(List.fill(n)(fa))

  def product[A,B](fa: F[A], fb: F[B]): F[(A,B)] = map2(fa, fb)((_,_))

  def map3[A,B,C,D](fa: F[A],
                    fb: F[B],
                    fc: F[C])(f: (A, B, C) => D): F[D] = ???
}

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))
  def join[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(fa => fa)
  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] = a => flatMap(f(a))(g)
  def map[B](fa: F[A])(f: A => B): F[B] = flatMap(fa)((a: A) => unit(f(a)))
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = flatMap(fa)(a => map(fb)(b => f(a,b)))
}

object Applicative {
  val streamApplicative = new Applicative[Stream] {

    def unit[A](a: => A): Stream[A] = Stream.continually(a)

    def map2[A,B,C](as: Stream[A], ab: Stream[B])(f: (A,B) => C): Stream[C] =
      as zip ab f.tupled

    def sequence[A](a: List[Stream[A]]): Stream[List[A]] =
      a.foldRight(unit(List[A]()))((sa, l) => map2(sa, l)(_ :: _))
  }
}