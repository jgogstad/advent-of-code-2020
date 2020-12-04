package jgogstad.utils

import cats.effect.concurrent.Deferred
import cats.syntax.all._
import cats.effect._
import cats.effect.syntax.all._
import cats.data.NonEmptyList

object RaceN {

  def raceN[F[_]: Concurrent, A](xs: NonEmptyList[F[A]]): F[Either[NonEmptyList[Throwable], A]] = {
    Deferred[F, A].flatMap { deferred =>
      val startAll = xs.traverse(fa => Concurrent[F].start(fa.flatMap(a => deferred.complete(a).as(a)).attempt))

      val race = Bracket[F, Throwable].bracket(startAll) { fibers: NonEmptyList[Fiber[F, Either[Throwable, A]]] =>
        Concurrent[F].race(
          fibers.traverse(_.join).map(_.traverse(_.swap).swap),
          deferred.get
        )
      }(_.traverse_(_.cancel.attempt))

      race.map(out => out.fold(identity, Either.right))
    }
  }
}
