trait Functor[F[_]] {
  def map[A,B](as: F[A])(f: A => B): F[B]
}

object Functor {

  val listFunctor = new Functor[List] {
    def map[A,B](as: List[A])(f: A => B): List[B] = as map f
  }

}