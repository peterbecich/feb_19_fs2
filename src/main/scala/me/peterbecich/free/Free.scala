package me.peterbecich.free

/*
 Taken from
 https://github.com/fpinscala/fpinscala/blob/master/answers/src/main/scala/fpinscala/iomonad/IO.scala#L359
 https://github.com/fpinscala/fpinscala/blob/master/answers/src/main/scala/fpinscala/iomonad/package.scala
 */

sealed trait Free[F[_],A] {
  def flatMap[B](f: A => Free[F,B]): Free[F,B] =
    FlatMap(this, f)
  def map[B](f: A => B): Free[F,B] =
    this.flatMap((a: A) => Return(f(a)))
}

case class Return[F[_],A](a: A) extends Free[F,A]
case class Suspend[F[_],A](s: F[A]) extends Free[F,A]
case class FlatMap[F[_],A,B](s: Free[F,A], f: A => Free[F,B]) extends Free[F,B]

object Free {
  import cats.Monad
  import scala.util.Either

  /*
   https://github.com/fpinscala/fpinscala/blob/master/answers/src/main/scala/fpinscala/iomonad/IO.scala#L378
   with kind-projector plugin
   https://github.com/non/kind-projector

   https://typelevel.org/cats/api/cats/Monad.html
   */
  // def freeMonad[F[_]]: Monad[Lambda[A => Free[F,A]]] =
  //   new Monad[Lambda[A => Free[F,A]]] {
  //     def pure[A](a: A) = Return(a)
  //     def flatMap[A,B](fa: Free[F,A])(f: A => Free[F, B]): Free[F,B] =
  //       fa.flatMap(f)

  //     // https://typelevel.org/cats/api/cats/Monad.html#tailRecM[A,B](a:A)(f:A=%3EF[Either[A,B]]):F[B]
  //     // http://eed3si9n.com/herding-cats/tail-recursive-monads.html
  //     def tailRecM[A,B](a: A)(f: (A) => Free[F, Either[A, B]]): Free[F,B] =
  //       f(a).map { _ match {
  //         case Right(b) => b
  //         // does *not* use constant stack space
  //         case Left(a2) => (tailRecM(a2)(f))
  //       }
  //       }
  //   }

  // val freeFunction0Monad = freeMonad[Function0]

  val freeFunction0Monad: Monad[Lambda[A => Free[Function0,A]]] =
    new Monad[Lambda[A => Free[Function0,A]]] {
      def pure[A](a: A) = Return(a)
      def flatMap[A,B](fa: Free[Function0,A])(f: A => Free[Function0, B]): Free[Function0,B] =
        fa.flatMap(f)

      // https://typelevel.org/cats/api/cats/Monad.html#tailRecM[A,B](a:A)(f:A=%3EF[Either[A,B]]):F[B]
      // http://eed3si9n.com/herding-cats/tail-recursive-monads.html
      def tailRecM[A,B](a: A)(f: (A) => Free[Function0, Either[A, B]]): Free[Function0,B] =
        f(a).map { _ match {
          case Right(b) => b
          // does *not* use constant stack space
          case Left(a2) => runTrampoline(tailRecM(a2)(f))
        }
        }
    }


  // A Free monad interpreter
  // https://github.com/fpinscala/fpinscala/blob/master/answers/src/main/scala/fpinscala/iomonad/IO.scala#L384
  @annotation.tailrec
  def runTrampoline[A](a: Free[Function0,A]): A = (a) match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x,f) => x match {
      case Return(a) => runTrampoline { f(a) }
      case Suspend(r) => runTrampoline { f(r()) }
      case FlatMap(a0,g) => runTrampoline { a0 flatMap { a0 => g(a0) flatMap f } }
    }
  }

  // https://github.com/fpinscala/fpinscala/blob/master/answers/src/main/scala/fpinscala/iomonad/IO.scala#L595
  type IO[A] = Free[Function0, A]



}

object FreeExamples extends App {

  // import cats.implicits._
  import Free.IO
  import Free.runTrampoline
  import Free.freeFunction0Monad

  val countToTenStart: IO[Int] = Return(0).flatMap(countToTenLoop)
  def countToTenLoop(x: Int): IO[Int] =
    if(x<11) Return(println("count is: "+x)).flatMap((_: Unit) => countToTenLoop(x+1))
    else Return(x)


  println("count to ten")
  runTrampoline(countToTenStart)

  // println("hello five times")
  // val hello: IO[Unit] = Return(println("hello!"))
  // val fiveHello: IO[List[Unit]] = freeFunction0Monad.replicateA(5, hello)

  // val fiveHello: IO[Unit] =
  //   freeFunction0Monad.iterateUntilM(0)((x: Int) => hello.map(_ => x+1))(_<5).map(_=>())
  // runTrampoline(fiveHello)

  // https://typelevel.org/cats/api/cats/Monad.html#iterateWhileM[A](init:A)(f:A=%3EF[A])(p:A=%3EBoolean):F[A]
  def countIncrement(x: Int): IO[Int] =
    Return(println("count is: "+x)).flatMap((_: Unit) => Return(x+1))
  val countToTwenty: IO[Int] = freeFunction0Monad.iterateWhileM(0)(countIncrement)(_ < 21)

  println("count to twenty")
  runTrampoline(countToTwenty)


}

