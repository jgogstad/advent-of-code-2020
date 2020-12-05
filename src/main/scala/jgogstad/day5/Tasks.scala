package jgogstad.day5

import fs2.Stream
import cats.effect.{Blocker, ExitCode, IO, IOApp}
import io.odin.{consoleLogger, Logger}
import cats.syntax.all._
import jgogstad.utils.FileIo

import scala.annotation.tailrec
import scala.util.{Failure, Try}

object Tasks extends IOApp {
  val log: Logger[IO] = consoleLogger()

  def decode(s: String, high: Int): Try[Int] = {
    @tailrec
    def go(low: Int, high: Int, rest: List[Char]): Try[Int] = {
      rest match {
        case 'F' :: Nil =>
          low.pure[Try]
        case 'B' :: Nil =>
          high.pure[Try]
        case 'F' :: t   =>
          go(low, (high - low) / 2 + low, t)
        case 'B' :: t   =>
          go((Math.ceil((high.toDouble - low) / 2) + low).toInt, high, t)
        case h :: _     => Failure(new Exception(show"Unexpected character '$h'"))
        case Nil        => Failure(new Exception("Empty list"))
      }
    }
    go(0, high, s.toList)
  }

  override def run(args: List[String]): IO[ExitCode] = Blocker[IO].use { blocker =>
    val parseSeats = FileIo.contentsOf[IO]("day5/input.txt", blocker)
      .map { s =>
        val (fb, lr) = s.span("FB".contains(_))
        fb -> lr.map {
          case 'L' => 'F'
          case 'R' => 'B'
          case s   => s
        }
      }
      .evalMap {
        case (fb, lr) => (decode(fb, 127), decode(lr, 7)).tupled.liftTo[IO]
      }
      .map(t => t._1 * 8 + t._2)


    val task1 = parseSeats.fold(0)(Math.max).evalTap(i => log.info(show"Task1: $i")).compile.lastOrError

    val task2 = parseSeats
      .foldMap(List.apply[Int](_))
      .map(_.sorted)
      .flatMap(Stream.emits)
      .zipWithPrevious
      .mapFilter {
        case (Some(p), c) => ((c - p) === 2).guard[Option].as(p + 1)
        case (None, _) => None
      }
      .compile
      .lastOrError
      .flatTap(r => log.info(show"Task2: $r"))

    (task1, task2).parTupled.as(ExitCode.Success)
  }

  private val exampleInput = Stream.emit("FBFBBFFRLR").covary[IO]
}
