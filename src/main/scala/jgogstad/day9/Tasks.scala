package jgogstad.day9

import cats.data.{Chain, NonEmptyChain}
import fs2.{Chunk, Stream}
import cats.effect.{Blocker, ExitCode, IO, IOApp}
import cats.syntax.all._

import scala.collection.immutable.Queue
import io.odin.{consoleLogger, Logger}
import jgogstad.utils.FileIo

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object Tasks extends IOApp {
  private val log: Logger[IO] = consoleLogger()

  private def sumMatrix(in: Queue[BigInt]): Queue[Queue[BigInt]] =
    in.zipWithIndex.map {
      case (e, i) => Queue(in.zipWithIndex.filter(_._2 =!= i).map(_._1 + e): _*)
    }

  private def shiftMatrixForWindow(
    mat: Queue[Queue[BigInt]],
    window: Queue[BigInt],
    el: BigInt
  ): Queue[Queue[BigInt]] = {
    val dequeue1     = mat.tail.map(_.tail).zip(window)
    val elementAdded = dequeue1.map(t => t._1.enqueue(t._2 + el))
    val nextQueue    = mat.tail.zip(window.tail).map(_._2 + el)

    elementAdded.enqueue[Queue[BigInt]](nextQueue)
  }

  def solveTask1(lookback: Int, data: Stream[IO, BigInt]): IO[BigInt] =
    data
      .sliding(lookback)
      .zip(data.drop(lookback.toLong))
      .scan((none[BigInt], Queue.empty[Queue[BigInt]])) {
        case ((_, acc), (window, el)) =>
          val matrix = if (acc.isEmpty) sumMatrix(window) else acc
          if (matrix.flatten.contains(el)) none[BigInt] -> shiftMatrixForWindow(matrix, window, el)
          else el.some                                  -> matrix
      }
      .map(_._1)
      .unNone
      .take(1)
      .compile
      .lastOrError

  def solveTask2(target: BigInt, data: Array[BigInt]): Try[BigInt] = {
    @tailrec
    def go(sum: BigInt, start: Int, end: Int): Try[(Int, Int)] = {
      if (sum === target) Success(start -> end)
      else if (end >= data.length) Failure(new Exception("No range found"))
      else if (sum > target) go(0, start + 1, start + 1)
      else if (sum < target)
        data.lift(end) match {
          case Some(v) => go(sum + v, start, end + 1)
          case None    => Failure(new Exception(show"Out of bounds, $end"))
        }
      else Failure(new Exception("Unexpected branch"))
    }

    for {
      (start, end) <- go(0, 0, 0)
      elements     <- Try(data.slice(start, end))
    } yield elements.min + elements.max
  }

  override def run(args: List[String]): IO[ExitCode] = Blocker[IO].use { blocker =>
    val exampleData = exampleInput.evalMap(i => Try(BigInt(i)).liftTo[IO])
    val task1Data = FileIo
      .contentsOf[IO]("day9/input.txt", blocker)
      .evalMap(i => Try(BigInt(i)).liftTo[IO])

    val example = solveTask1(5, exampleData).flatTap(i => log.info(show"Example: $i"))
    val task1   = solveTask1(25, task1Data).flatTap(i => log.info(show"Task1: $i"))

    val task2 = (task1, task1Data.compile.toList.map(_.toArray)).tupled
      .map((solveTask2 _).tupled)
      .flatMap(_.liftTo[IO])
      .flatTap(i => log.info(show"Task2: $i"))

    (example, task1, task2).tupled.as(ExitCode.Success)
  }

  private val exampleInput: Stream[IO, String] = {
    val data = """35
                 |20
                 |15
                 |25
                 |47
                 |40
                 |62
                 |55
                 |65
                 |95
                 |102
                 |117
                 |150
                 |182
                 |127
                 |219
                 |299
                 |277
                 |309
                 |576""".stripMargin
    Stream.emits(data.split("\n")).covary[IO]
  }
}
