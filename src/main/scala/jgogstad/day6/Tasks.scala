package jgogstad.day6

import cats.effect.{Blocker, ExitCode, IO, IOApp}
import fs2.Stream
import cats.syntax.all._
import io.odin.{consoleLogger, Logger}
import jgogstad.utils.FileIo

object Tasks extends IOApp {
  private val log: Logger[IO] = consoleLogger()

  override def run(args: List[String]): IO[ExitCode] = Blocker[IO].use { blocker =>
//    val groups = exampleData
    val groups: Stream[IO, List[String]] = FileIo.contentsOf[IO]("day6/input.txt", blocker)
      .groupAdjacentBy(_.nonEmpty)
      .mapFilter(t => t._1.guard[Option].as(t._2.toList))

    val task1 = groups
      .map(_.toSet.flatMap((s: String) => s.toCharArray.toSet))
      .foldMap(_.size)
      .evalTap(l => log.info(show"Task1: $l"))
      .compile
      .lastOrError

    val task2 = groups
      .map(ms => ms.length -> ms.flatMap((s: String) => s.toCharArray.toList))
      .map { case (size, data) => size -> data.groupMap(identity)(_ => 1).view.mapValues(_.size).toMap } // make hist.
      .foldMap { case (size, cs) => cs.filter(_._2 === size).toList.foldMap(_ => 1) }
      .evalTap(l => log.info(show"Task2: $l"))
      .compile
      .lastOrError

    (task1, task2).parTupled.as(ExitCode.Success)
  }

  private val exampleData: Stream[IO, String] = {
      val data = """abc
      |
      |a
      |b
      |c
      |
      |ab
      |ac
      |
      |a
      |a
      |a
      |a
      |
      |b""".stripMargin
    Stream.emits(data.split("\n")).covary[IO]
  }
}
