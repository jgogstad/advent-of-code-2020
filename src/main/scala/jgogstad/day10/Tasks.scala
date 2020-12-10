package jgogstad.day10

import cats.syntax.all._

import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Failure, Success}

object Tasks extends App {
  val input = Source.fromResource("day10/input.txt")
    .getLines()
    .toList
    .traverse(i => i.toIntOption.toRight(new Exception(show"Not an int: $i")))
    .toTry
    .map(_.sorted(Ordering[Int].reverse))
    .flatMap {
      case h :: t => Success((h + 3) :: h :: t)
      case Nil => Failure(new Exception("Empty input"))
    }
    .map(l => 0 :: l.reverse)

  @tailrec
  def count(last: Int, diffs: Map[Int, Int], stack: List[Int]): Option[Int] =
    stack match {
      case h :: t => count(h, diffs.updatedWith(h - last)(_.map(_ + 1).orElse(1.some)), t)
      case Nil => (diffs.get(1), diffs.get(3)).tupled.map(((_:Int) * (_: Int)).tupled)
    }

  @tailrec
  def countPaths(memo: Map[Int, BigInt], stack: List[Int], data: Array[Int]): Option[BigInt] = {
    stack match {
      case Nil => memo.get(0)
      case h :: t => Option(data.indexOf(h)).filter(_ >= 0) match {
        case Some(index) =>
          val adapters = data.drop(index).takeWhile(_ <= h + 3).filter(_ > h).toList
          adapters match {
            case Nil => countPaths(memo + (h -> 1), t, data)
            case ls => ls.traverse(memo.get).map(_.sum) match {
              case Some(v) => countPaths(memo + (h -> v), t, data)
              case None => countPaths(memo, adapters ::: stack, data)
            }
          }
        case None => None
      }
    }
  }

  input.map { input =>
    val task1 = count(0, Map.empty, input)
    val task2 = countPaths(Map.empty, List(0), input.toArray)
    println(task1)
    println(task2)
  }.get
}
