
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
Par.map2[A,B,C](pa: Par[A], pb: Par[B])(f: (A,B) => C): Par[C] = ???

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
}