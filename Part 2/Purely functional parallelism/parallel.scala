
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

