import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}
import java.util.concurrent.atomic.AtomicReference

object Nonblocking {

  // non-blocking Future
  trait Future[A] {
    def apply(k: A => Unit): Unit
  }
  type Par[+A] = ExecutorService => Future[A]

}