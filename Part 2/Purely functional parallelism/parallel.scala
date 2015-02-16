
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
Par.map2[A,B,C](pa: Par[A], Par[B])(f: (A,B) => C): Par[C] = ???