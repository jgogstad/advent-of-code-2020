package jgogstad.day17

import cats.syntax.all._

import scala.annotation.tailrec
import scala.io.Source

object Tasks extends App {

  val data = Source.fromResource("day17/input.txt").getLines().toList

  def solve(in: List[String], dimensions: Int): Set[List[Int]] = {
    val cross = {
      @tailrec
      def calculate(r: Set[List[Int]], i: Int): Set[List[Int]] = {
        if (i === dimensions) r
        else calculate(r.flatMap(el => Set(-1, 0, 1).map(_ :: el)), i + 1)
      }

      calculate(Set(List(-1, 0, 1)), 0)
    }

    def cubeAt(coords: List[Int]): Set[List[Int]] =
      cross.map(_.zip(coords).map(((_: Int) + (_: Int)).tupled)).filter(_ =!= coords)

    @tailrec
    def go(iterations: Int, space: Set[List[Int]]): Set[List[Int]] = {
      if (iterations <= 0) space
      else {
        val emptySpace: Set[List[Int]] = space.flatMap(cubeAt).diff(space)

        val keep = space.filter { t =>
          val activeCount = cubeAt(t).count(space.contains)
          activeCount >= 2 && activeCount <= 3
        }

        val set = emptySpace.filter { t =>
          val activeCount = cubeAt(t).count(space.contains)
          activeCount === 3
        }

        go(iterations - 1, keep ++ set)
      }
    }

    def parse(in: List[String], dimensions: Int): Set[List[Int]] =
      in.zipWithIndex.flatMap {
        case (s, x) =>
          s.zipWithIndex.collect {
            case ('#', y) => List(x, y).padTo(dimensions, 0)
          }
      }.toSet

    go(6, parse(in, dimensions))
  }

  val t1 = solve(data, 3)
  println(show"Task1: ${t1.size}")

  val t2 = solve(data, 4)
  println(show"Task2: ${t2.size}")
}
