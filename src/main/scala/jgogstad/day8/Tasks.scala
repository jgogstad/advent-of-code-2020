package jgogstad.day8

import cats.data.{NonEmptyList, NonEmptySet}
import fs2.Stream
import cats.effect.{Blocker, ExitCode, IO, IOApp}
import cats.syntax.all._
import io.odin.{consoleLogger, Logger}
import jgogstad.utils.{FileIo, RaceN}

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object Tasks extends IOApp {

  private val log: Logger[IO] = consoleLogger()

  object int { def unapply(s: String): Option[Int] = s.toIntOption }

  private def execute(instructions: Array[String]): Try[(Boolean, Int)] = {
    @tailrec
    def go(acc: Int, ip: Int, executed: Set[Int]): Try[(Boolean, Int)] = {
      if (executed(ip)) Success(false -> acc)
      else
        instructions.lift(ip) match {
          case Some(s"nop ${int(_)}")             => go(acc, ip + 1, executed + ip)
          case Some(s"acc ${int(v)}")             => go(acc + v, ip + 1, executed + ip)
          case Some(s"jmp ${int(v)}")             => go(acc, ip + v, executed + ip)
          case Some(i)                            => Failure(new Exception(show"Unrecognized input $i"))
          case None if ip === instructions.length => Success(true -> acc)
          case None                               => Failure(new Exception(show"Instruction pointer out of bounds, $ip"))
        }
    }
    go(0, 0, Set.empty)
  }

  private def solveTask2(instructions: Array[String]): IO[Int] = {
    val permutations = instructions.zipWithIndex.foldLeft(List(instructions)) {
      case (acc, (el, idx)) =>
        el match {
          case s"nop $num" => instructions.updated(idx, show"jmp $num") :: acc
          case s"jmp $num" => instructions.updated(idx, show"nop $num") :: acc
          case _           => acc
        }
    }

    val executions = permutations.map { i =>
      IO(execute(i)).flatMap {
        case Success((true, i))  => IO.pure(i)
        case Success((false, _)) => IO.raiseError(new Exception("Didn't finish"))
        case Failure(t)          => IO.raiseError(t)
      }
    }

    NonEmptyList.fromList(executions) match {
      case Some(es) => RaceN.raceN(es).map(_.leftMap(_.head)).rethrow
      case None     => IO.raiseError(new Exception("Got no input"))
    }
  }

  override def run(args: List[String]): IO[ExitCode] = Blocker[IO].use { blocker =>
    val task1 = FileIo
      .contentsOf[IO]("day8/input.txt", blocker)
      .compile
      .toList
      .map(_.toArray)
      .flatMap(execute(_).liftTo[IO])
      .map(_._2)
      .flatTap(i => log.info(show"Task1: $i"))

    val task2 = FileIo
      .contentsOf[IO]("day8/input.txt", blocker)
      .compile
      .toList
      .map(_.toArray)
      .flatMap(solveTask2)
      .flatTap(i => log.info(show"Task2: $i"))

    (task1, task2).tupled.as(ExitCode.Success)
  }

  private val example: Stream[IO, String] = {
    val data = """nop +0
                 |acc +1
                 |jmp +4
                 |acc +3
                 |jmp -3
                 |acc -99
                 |acc +1
                 |jmp -4
                 |acc +6""".stripMargin
    Stream.emits(data.split("\n")).covary[IO]
  }
}
