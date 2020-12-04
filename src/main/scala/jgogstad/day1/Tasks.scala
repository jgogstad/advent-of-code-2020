package jgogstad.day1

import cats.data.NonEmptyList
import cats.effect.{Blocker, ExitCode, IO, IOApp}
import cats.syntax.all._
import io.odin.{consoleLogger, Logger}
import jgogstad.utils.{FileIo, RaceN}

import scala.annotation.nowarn

object Tasks extends IOApp {
  val log: Logger[IO] = consoleLogger()

  override def run(args: List[String]): IO[ExitCode] = Blocker[IO].use { blocker =>
    val readInput = FileIo
      .contentsOf[IO]("day1/input.txt", blocker)
      .evalMap(i => IO(i.toInt))
      .compile
      .toList
      .flatMap {
        case h :: t => NonEmptyList.of(h, t: _*).pure[IO]
        case Nil => IO.raiseError[NonEmptyList[Int]](new Exception("Empty file"))
      }

    def sum2(target: Int, input: NonEmptyList[Int]): IO[(Int, Int)] = {
      val diffSet: Set[Int] = input.foldMap(i => Set(target - i))
      val rhs               = input.find(diffSet.contains)
      rhs.toRight(new Exception("No pair found")).liftTo[IO].map(rhs => rhs -> (target - rhs))
    }

    def sum3(target: Int, input: NonEmptyList[Int]): IO[(Int, Int, Int)] = {
      val calculations = input.map(i => sum2(target - i, input).map(t => (i, t._1, t._2)))
      RaceN.raceN[IO, (Int, Int, Int)](calculations).map(_.leftMap(_.head)).rethrow
    }

    val task1 = for {
      input      <- readInput
      (lhs, rhs) <- sum2(2020, input) // 651651
      _          <- log.info(show"Task1 product: $lhs * $rhs = ${lhs * rhs}")
    } yield ExitCode.Success

    val task2 = for {
      input             <- readInput
      (one, two, three) <- sum3(2020, input) // 241861950
      _                 <- log.info(show"Task 2 product: $one * $two * $three = ${one * two * three}")
    } yield ExitCode.Success

    task2
  }

  private val exampleInput = IO(List(1721, 979, 366, 299, 675, 1456))
}
