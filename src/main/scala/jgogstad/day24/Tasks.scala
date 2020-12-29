package jgogstad.day24

import spire.math.Complex
import spire.std.double._
import cats.syntax.all._

import scala.annotation.tailrec
import scala.io.Source

object Tasks extends App {

  val se = Complex(0.5, -0.5)
  val sw = Complex(-0.5, -0.5)
  val nw = Complex(-0.5, 0.5)
  val ne = Complex(0.5, 0.5)
  val w  = Complex(-1.0, 0)
  val e  = Complex(1.0, 0)

  def parse(s: String): List[Complex[Double]] = {
    def go(r: List[Complex[Double]], rest: List[Char]): List[Complex[Double]] =
      rest match {
        case Nil             => r.reverse
        case 's' :: 'e' :: t => go(se :: r, t)
        case 's' :: 'w' :: t => go(sw :: r, t)
        case 'n' :: 'w' :: t => go(nw :: r, t)
        case 'n' :: 'e' :: t => go(ne :: r, t)
        case 'w' :: t        => go(w :: r, t)
        case 'e' :: t        => go(e :: r, t)
      }
    go(List.empty, s.toCharArray.toList)
  }

  def task1(in: List[String]): Int =
    in.map(parse)
      .map(_.reduce(_ + _))
      .groupBy(identity)
      .count { case (_, v) => v.length % 2 == 1 }

  def task2(in: List[String], rounds: Int): Int = {
    val operations = in.map(parse).map(_.reduce(_ + _))
    val blacks = operations
      .groupBy(identity)
      .toList
      .mapFilter(t => (t._2.length % 2 == 1).guard[Option].as(t._1))

    def adjacent(t: Complex[Double]): Set[Complex[Double]] = List(e, se, sw, w, nw, ne).map(_ + t).toSet

    @tailrec
    def go(black: Set[Complex[Double]], i: Int): Set[Complex[Double]] = {
      val whites = black.flatMap(adjacent(_).diff(black))

      val blackToWhite = black.filter { t =>
        val n = adjacent(t).count(black.contains)
        n === 0 || n > 2
      }

      val whiteToBlack = whites.filter(adjacent(_).count(black.contains) === 2)

      if (i === rounds) black
      else go((black -- blackToWhite) ++ whiteToBlack, i + 1)
    }

    go(blacks.toSet, 0).size
  }

  val input = Source.fromResource("day24/input.txt").getLines().toList

  println(s"Task 1: ${task1(input)}")
  println(s"Task 2: ${task2(input, 100)}")
}
