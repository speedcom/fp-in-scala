trait Functor[F[_]] {

  def map[A,B](as: F[A])(f: A => B): F[B]

  def distribute[A,B](as: F[(A,B)]): (F[A], F[B]) = (map(as)(_._1), map(as)(_._2))

  def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {

  val listFunctor = new Functor[List] {
    def map[A,B](as: List[A])(f: A => B): List[B] = as map f
  }
