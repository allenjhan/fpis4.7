object EitherTraverseTest extends App {
  val myEitherList = List(Right(1), Right(2), Right(3))
  println(Either.sequence(myEitherList))
  val myList = List(1, 2, 3)
  println(Either.traverse(myList)(x => if(x%2==1) Right(x) else Left(new IllegalArgumentException("Even value encountered"))))
}


sealed trait Either[+E,+A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(value) => Left(value)
    case Right(value) => Right(f(value))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(value) => Left(value)
    case Right(value) => f(value)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(value) => b
    case Right(value) => Right(value)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this match {
    case Left(avalue) => Left(avalue)
    case Right(avalue) => b match {
      case Left(bvalue) => Left(bvalue)
      case Right(bvalue) => Right(f(avalue,bvalue))
    }
  }
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    es.foldRight(Right(Nil):Either[E, List[B]]){(e, acc) =>
      for {
        a <- acc;
        b <- f(e)
      } yield b::a
    }
  }

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] = traverse(es)(identity)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}