package jgogstad.day3

import cats.effect.{Blocker, ExitCode, IO, IOApp}
import cats.syntax.all._
import fs2.Stream
import io.odin.{consoleLogger, Logger}
import jgogstad.utils.FileIo

import scala.util.{Success, Try}

object Task extends IOApp {
  val log: Logger[IO] = consoleLogger()

  override def run(args: List[String]): IO[ExitCode] = Blocker[IO].use { blocker =>
    val readInput = FileIo.contentsOf[IO]("day3/input.txt", blocker)

    def countTrees(right: Int, down: Int): IO[BigInt] =
      readInput
        .zipWithIndex
        .filter(_._2 % down === 0)
        .map {
          case (line, index) =>
            val treeIndex = (index.toInt / down * right) % line.length
            Try(line(treeIndex)) match {
              case Success('#') => 1
              case _            => 0
            }
        }
        .foldMonoid
        .map(BigInt.apply)
        .compile
        .lastOrError

    val counts = (
      countTrees(1, 1),
      countTrees(3, 1),
      countTrees(5, 1),
      countTrees(7, 1),
      countTrees(1, 2)
    )

    counts.parTupled
      .flatMap {
        case (oneOne, threeOne, fiveOne, sevenOne, oneTwo) =>

          log.info(
            show"""Result:
                  |1-1: $oneOne
                  |3-1: $threeOne
                  |5-1: $fiveOne
                  |7-1: $sevenOne
                  |1-2: $oneTwo
                  |
                  |$oneOne * $threeOne * $fiveOne * $sevenOne * $oneTwo = ${oneOne * threeOne * fiveOne * sevenOne * oneTwo} """.stripMargin
          )
      }
      .as(ExitCode.Success)
  }

  val exampleInput: Stream[IO, String] = {
    val data = """..##.........##.........##.........##.........##.........##.......
                 |#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..#...#...#..
                 |.#....#..#..#....#..#..#....#..#..#....#..#..#....#..#..#....#..#.
                 |..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#..#.#...#.#
                 |.#...##..#..#...##..#..#...##..#..#...##..#..#...##..#..#...##..#.
                 |..#.##.......#.##.......#.##.......#.##.......#.##.......#.##.....
                 |.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#.#.#.#....#
                 |.#........#.#........#.#........#.#........#.#........#.#........#
                 |#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...#.##...#...
                 |#...##....##...##....##...##....##...##....##...##....##...##....#
                 |.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#.#..#...#.#""".stripMargin

    Stream.emits(data.split("\n")).covary[IO]
  }
}
