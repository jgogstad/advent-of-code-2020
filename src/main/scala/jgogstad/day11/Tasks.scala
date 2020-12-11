package jgogstad.day11
import cats.syntax.all._

import scala.annotation.tailrec
import scala.io.Source

object Tasks extends App {

  def occupiedAdjacent(data: Array[String], i: Int, j: Int): Int =
    (i - 1)
      .to(i + 1)
      .flatMap(i => (j - 1).to(j + 1).map(j => (i, j)))
      .filter(t => t._1 =!= i || t._2 =!= j)
      .map(t => data.lift(t._1).flatMap(_.lift(t._2)).getOrElse('.'))
      .count(_ === '#')

  def occupiedFirst(data: Array[String], i: Int, j: Int): Int = {
    @tailrec
    def scan(ii: Int, jj: Int, nextRow: Int => Int, nextCol: Int => Int): Int = {
      data.lift(ii).flatMap(s => s.lift(jj)) match {
        case Some('.') => scan(nextRow(ii), nextCol(jj), nextRow, nextCol)
        case Some('#') => 1
        case Some('L') => 0
        case _         => 0
      }
    }

    val inc: Int => Int = _ + 1
    val dec: Int => Int = _ - 1
    val id              = identity[Int] _

    val ops          = List(dec, inc, id)
    val crossProduct = ops.flatMap(f => ops.map(f -> _))

    crossProduct
      .filter(t => t._1 != id || t._2 != id)
      .map {
        case (f, g) => scan(f(i), g(j), f, g)
      }
      .sum
  }

  def solve(occupiedLimit: Int, data: Array[String], neigbours: (Array[String], Int, Int) => Int): Int = {
    @tailrec
    def go(previousChanges: Int, last: Array[String]): Int = {
      if (previousChanges === 0) last.map(_.filter(_ === '#').length).sum
      else {
        var updated = 0
        val nextState = last.zipWithIndex.map {
          case (row, i) =>
            row.zipWithIndex.map {
              case ('.', _) => '.'
              case ('L', j) if neigbours(last, i, j) === 0 =>
                updated += 1
                '#'
              case ('#', j) if neigbours(last, i, j) >= occupiedLimit =>
                updated += 1
                'L'
              case (c, _) => c
            }
        }
        go(updated, nextState.map(_.mkString))
      }
    }
    go(-1, data)
  }

  def task1(data: Array[String]): Int = solve(4, data, occupiedAdjacent)
  def task2(data: Array[String]): Int = solve(5, data, occupiedFirst)

  val data = Source.fromResource("day11/input.txt").getLines().toArray
  println(s"Task 1: ${task1(data)}")
  println(s"Task 2: ${task2(data)}")

}
