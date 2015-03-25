case class Player(name: String, score: Int)

def contest(p1: Player, p2: Player): Unit =
  if(p1.score > p2.score)
    println(s"${p1.name} is the winner")
  else if(p1.score < p2.score)
    println("s{p2.name} is the winner")
  else
    println("It's a draw")

def winner(p1: Player, p2: Player): Option[Player] = {
  if(p1.score > p2.score) Some(p1)
  else if(p1.score < p2.score) Some(p2)
  else None
}

def contest_(p1: Player, p2: Player): Unit = winner(p1, p2) match {
  case Some(Player(name, _)) => println(s"${name} is the winner")
  case None => println("It's a draw")
}

def winnerMsg(p: Option[Player]): String = p map {
  case Player(name, _) => "${name is the winner"
} getOrElse("It's a draw")

def contest__(p1: Player, p2: Player): Unit = println(winnerMsg(winner(p1, p2)))

trait IO {
  def run: Unit
}

def PrintLine(msg: String): IO = new IO { def run = println(msg) }

def contest___(p1: Player, p2: Player): IO = PrintLine(winnerMsg(winner(p1,p2)))

def fahrenheitToCelsius(f: Double): Double = (f - 32) * 5.0/9.0

def converter: Unit = {
  println("Enter a temperature in degrees Fahrenheit: ")
  val d = readLine.toDouble
  println(fahrenheitToCelsius(d))
}

sealed trait IO[A] { self =>
  def run: A
  def map[B](f: A => B): IO[B] = new IO[B] { def run: B = f(self.run) }
  def flatMap[B](f: A => IO[B]): IO[B] = new IO[B] { def run: B = f(self.run).run }
}

object IO extends Monad[IO] {
  def unit[A](a: => A): IO[A] = new IO[A] { def run: A = a }
  def flatMap[A,B](fa: IO[A])(f: A => IO[B]): IO[B] = fa.flatMap(f)
  def apply[A](a: => A): IO[A] = unit(a)
}

def ReadLine: IO[String] = IO { readLine }
def PrintLine(msg: String): IO[Unit] = IO { println(msg) }

def converter: IO[Unit] = for {
  _ <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
  d <- ReadLine.map(_.toDouble)
  _ <- PrintLine(fahrenheitToCelsius(d).toString)
} yield ()

// An `IO[Unit]` that reads a line from the console and echoes it back.
val echo = ReadLine.flatMap(PrintLine)

// Parses an `Int` by reading a line from the console.
val readInt = ReadLine.map(_.toInt)

