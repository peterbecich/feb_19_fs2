package me.peterbecich.process

// https://github.com/fpinscala/fpinscala/blob/master/answers/src/main/scala/fpinscala/streamingio/StreamingIO.scala

import me.peterbecich.free._
import me.peterbecich.free.Free._
import scala.util.Either
//import scala.util.Option

import cats.Monad

// https://github.com/fpinscala/fpinscala/blob/master/answers/src/main/scala/fpinscala/streamingio/StreamingIO.scala#L514
sealed trait Process[O] {


  // def repeat: Process[O] = {
  //   def go(p: Process[O]): Process[O] = p match {
  //     case Halt() => go(this)
  //     case Await(recv) => Await {
  //       case None => recv(None)
  //       case i => go(recv(i))
  //     }
  //     case Emit(h, t) => Emit(h, go(t))
  //   }
  //   go(this)
  // }

  def Try[F[_],O](p: => Process[O]): Process[O] =
    try p
    catch { case e: Throwable => Halt() }

  // def onHalt(f: Throwable => Process[O]): Process[O] = this match {
  //   case Halt(e) => Try(f(e))
  //   case Emit(h, t) => Emit(h, t.onHalt(f))
  //   case Await(req,recv) => Await(req, recv andThen (_.onHalt(f)))
  // }
  

  // def ++(p: => Process[O]): Process[O] =
  //   this.onHalt {
  //     case End => Try(p) // we consult `p` only on normal termination
  //     case err => Halt(err)
  //   }


}

// https://github.com/fpinscala/fpinscala/blob/master/answers/src/main/scala/fpinscala/streamingio/StreamingIO.scala#L704
case class Await[A, O](
  req: IO[A],
  recv: Either[Throwable,A] => Process[O]) extends Process[O]

case class Emit[O](
  head: O,
  tail: Process[O] = Halt[O]()) extends Process[O]

case class Halt[O]() extends Process[O]


object Process {

  def emit[O](head: O, tail: Process[O] = Halt[O]()): Process[O] =
    Emit(head, tail)

  def await[A, O](
    req: IO[A]
  )(
    recv: Either[Throwable, A] => Process[O]
  ): Process[O] = Await(req, recv)



}



/*

https://github.com/functional-streams-for-scala/fs2/blob/series/0.10/core/shared/src/main/scala/fs2/internal/Algebra.scala#L14-L42

 */
