// a) (A => B )   => (C[A] => C[B]) // Functor
// b) (A => C[B]) => (C[A] => C[B]) // Monad
// b) (C[A => B]) => (C[A] => C[B]) // Applicative

class MyBox[T](val value: T)

// FUNCTOR
def map[A,B](func: A => B): MyBox[A] => MyBox[B] = (a: MyBox[A]) => new MyBox(func(a.value))

val boxedstring: MyBox[String] = new MyBox("Hello")

def rawLengthOf(a: String): Int = a.length // rawLengthOf _ == val rlof: String => Int = (a: String) => a.length

val transformedLengthOfWithMap = map(rawfunc)

val result: MyBox[Int] = transformedLengthOfWithMap(boxedstring)

def lengthOf(a: String) = new MyBox(a.length)

// MONAD
def flatMap[A,B](func: A => MyBox[B]): MyBox[A] => MyBox[B] = (a: MyBox[A]) => func(a.value)

val transformedLengthOfWithFlatMap = flatMap(lengthOf)

val result2: MyBox[Int] = transformedLengthOfWithFlatMap(boxedstring)

val boxedLengthOf: MyBox[String=>Int] = new MyBox(rawLengthOf _)

// APPLICATIVE
def apply[A,B](ab: MyBox[A => B]): MyBox[A] => MyBox[B] = (a: MyBox[A]) => new MyBox(ab.value(a.value))

val transformedLengthOfWithApplicative = apply(new MyBox(rawLengthOf _))
val result3: MyBox[Int] = transformedLengthOfWithApplicative(boxedstring)

class MyBox[T](val value:T) {
   override def toString() = "MyBox("+value+")"
}

class MyBoxWrapper[T](w:MyBox[T]) {
    def flatMap[R](f:T=>MyBox[R]): MyBox[R] = map(f).value
    def map[R](f:T=>R): MyBox[R] = new MyBox(f(w.value))
}