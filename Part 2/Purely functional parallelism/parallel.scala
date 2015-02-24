
val sum: Int = (1 to 1000000).foldLeft(0)(_+_)

// divide-and-conquer
def sum(ints: IndexedSeq[Int]): Int = {
  if(ints.size <= 1)
    ints.headOption.getOrElse(0)
  else {
    val (l,r) = ints.splitAt(ints.size/2)
    sum(l) + sum(r)
  }
}

// EX 7.1
def unit[A](a: A): Par[A] = ???
def lazyUnit[A](a: => A): Par[A] = fork(unit(a))
def get[A](a: Par[A]): A = ???
def fork[A](a: => Par[A]): Par[A] = ???

// EX 7.2
import java.util.concurrent._

// basic implementation for Par
object Par {

  type Par[A] = ExecutorService => Future[A]

  // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future`
  // that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled.
  // Its `get` method simply returns the value that we gave it.
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone() = true
    def isCancelled() = !isDone()
    def cancel(evenIfRunning: Boolean): Boolean = false
    def get(timeout: Long, units: TimeUnit) = get
  }

  /*
  map2 doesn’t evaluate the call to f in a separate logical thread, in accord
  with our design choice of having fork be the sole function in the API for
  controlling parallelism. We can always do fork(map2(a,b)(f)) if we
  want the evaluation of f to occur in a separate thread.

  This implementation of map2 does not respect timeouts. It simply passes the
  ExecutorService on to both Par values, waits for the results of the Futures af and
  bf, applies f to them, and wraps them in a UnitFuture. In order to respect timeouts,
  we’d need a new Future implementation that records the amount of time spent evaluating
  af, and then subtracts that time from the available time allocated for evaluating bf.
  */
  def map2[A,B,C](pa: Par[A], pb: Par[B])(f: (A,B) => C): Par[C] = (es: ExecutorService) => {
    val fa = pa(es)
    val fb = pb(es)
    UnitFuture(f(fa.get, fb.get))
  }

  def fork[A](a: => Par[A]): Par[A] = (es: ExecutorService) => {
    es.submit(new Callable[A] { def call = a(es).get })
  }

  // EX 7.4
  def asyncF[A,B](f: A => B): A => Par[B] = a => unit(f(a))

  def map[A,B](pa: Par[A])(f: A => B) = map2(pa, unit(()))((a, _) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  // EX 7.5
  def sequence[A](ps: List[Par[A]]): Par[List[A]] = ps.foldRight[Par[List[A]]](unit())((h,t) => map2(h,t)(_ :: _))

}