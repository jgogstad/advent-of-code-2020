package jgogstad.day2

import cats.effect.{Blocker, ExitCode, IO, IOApp}
import cats.syntax.all._
import fs2.Stream
import io.odin.{consoleLogger, Logger}
import jgogstad.utils.FileIo

import scala.util.Try

object Tasks extends IOApp {
  val log: Logger[IO] = consoleLogger()

  def parse(s: String): Option[(Int, Int, Char, String)] =
    "(\\d+)-(\\d+) ([a-z]): (\\w+)".r.findFirstMatchIn(s).map { m =>
      (m.group(1).toInt, m.group(2).toInt, m.group(3).charAt(0), m.group(4))
    }

  override def run(args: List[String]): IO[ExitCode] = Blocker[IO].use { blocker =>
    val readInput = FileIo.contentsOf[IO]("day2/input.txt", blocker)

    val task1 = {
      val countOccurrences = readInput
        .mapFilter(parse)
        .filter {
          case (start, end, char, data) =>
            val length = data.filter(_ === char).length
            (length >= start) && (length <= end)
        }
        .foldMap(_ => 1)
        .compile
        .lastOrError

      for {
        r <- countOccurrences
        _ <- log.info(show"OK: $r")
      } yield ExitCode.Success
    }

    val task2 = {
      val policy = readInput
        .mapFilter(parse)
        .mapFilter {
          case (start, end, char, data) =>
            (Try(data(start - 1)), Try(data(end - 1)))
              .mapN { (c1, c2) => (c1 =!= c2) && (char === c1 || char === c2) }
              .toOption
              .map {
                case true  => 1
                case false => 0
              }
        }
        .foldMonoid
        .compile
        .lastOrError

      for {
        r <- policy
        _ <- log.info(show"OK: $r")
      } yield ExitCode.Success
    }

    task2
  }

  private val exampleInput = Stream
    .emits(
      List(
        "1-3 a: abcde",
        "1-3 b: cdefg",
        "2-9 c: ccccccccc"
      )
    )
    .covary[IO]
}
