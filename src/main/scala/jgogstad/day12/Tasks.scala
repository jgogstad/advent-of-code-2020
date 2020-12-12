package jgogstad.day12

import cats.syntax.all._
import spire.implicits._
import spire.math.Complex

import scala.io.Source
import scala.util.Try

object Tasks extends App {

  val parseInput =
    Source
      .fromResource("day12/input.txt")
      .getLines()
      .toList
      .mapFilter { s =>
        "([A-Z])(\\d+)".r
          .findFirstMatchIn(s)
          .flatMap(m => (Try(m.group(1).head), Try(m.group(2).toInt)).tupled.toOption)
      }

  def toComplex(c: Char): Complex[Int] = c match {
    case 'N' => Complex(0, 1)
    case 'S' => Complex(0, -1)
    case 'E' => Complex(1, 0)
    case 'W' => Complex(-1, 0)
    case _   => Complex(0, 0)
  }

  def move(point: Complex[Int], loc: Complex[Int], dir: Char, amount: Int): Option[(Complex[Int], Complex[Int])] =
    dir match {
      case 'F' => (point, loc + point * amount).some
      case 'R' => (point * List.fill(amount / 90)(Complex(0, -1)).reduce(_ * _), loc).some
      case 'L' => (point * List.fill(amount / 90)(Complex(0, 1)).reduce(_ * _), loc).some
      case _   => None
    }

  val (_, task1) = parseInput.foldLeft((toComplex('E'), Complex(0, 0))) {
    case ((direction, acc), (d, v)) => move(direction, acc, d, v).getOrElse((direction, acc + toComplex(d) * v))
  }

  val (_, task2) = parseInput.foldLeft((toComplex('E') * Complex(10, 1), Complex(0, 0))) {
    case ((waypoint, acc), (d, v)) => move(waypoint, acc, d, v).getOrElse((waypoint + toComplex(d) * v, acc))
  }

  println(s"Task1: $task1: ${task1.real.abs + task1.imag.abs}")
  println(s"Task1: $task2: ${task2.real.abs + task2.imag.abs}")
}
