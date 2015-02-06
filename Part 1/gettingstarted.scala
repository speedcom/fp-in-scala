// EX 2.2
val intComparator: (Int, Int) => Boolean = (a1, a2) => a1 <= a2

def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
  if(as.length == 1) true
  else as.sliding(2).map(a => ordered(a(0), a(1))).find(_ == false).getOrElse(true)
}

def partial1[A,B,C](a: A, f: (A,B) => C): B => C = (b: B) => f(a, b)

// EX 2.3
def curry[A,B,C](f: (A, B) => C): A => (B => C) = (a: A) => (b: B) => f(a,b)

// EX 2.4
def uncurry[A,B,C](f: A => B => C): (A,B) => C = f(a)(b)

// EX 2.5
def compose[A,B,C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))