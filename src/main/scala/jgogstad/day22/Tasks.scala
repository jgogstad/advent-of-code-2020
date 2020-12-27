package jgogstad.day22

import cats.data.Chain
import cats.syntax.all._
import fs2.Stream

import scala.annotation.tailrec
import scala.io.Source

object Tasks extends App {

  def parse(in: List[String]): (Chain[Int], Chain[Int]) = {
    val players = Stream
      .emits(in)
      .groupAdjacentBy(_.nonEmpty)
      .mapFilter(t => t._1.guard[Option].as(t._2.toList.tail))
      .map(ss => Chain.fromSeq(ss.map(_.toInt)))
      .compile
      .toList

    players match {
      case one :: two :: Nil => one -> two
      case _                 => throw new Exception("Invalid input")
    }
  }


  def task1(in: (Chain[Int], Chain[Int])): Int = {
    @tailrec
    def game(p1: Chain[Int], p2: Chain[Int]): Chain[Int] = {
      (p1.uncons, p2.uncons) match {
        case (Some((p1h, p1t)), Some((p2h, p2t))) =>
          if (p1h > p2h) game(p1t ++ Chain(p1h, p2h), p2t)
          else game(p1t, p2t ++ Chain(p2h, p1h))
        case (None, Some(_)) => p2
        case (Some(_), None) => p1
        case (None, None)    => throw new Exception("Inconceivable!")
      }
    }

    (game _).tupled(in).reverse.zipWithIndex.map {
      case (el, i) => el * (i + 1)
    }.toList.sum
  }

  def task2(in: (Chain[Int], Chain[Int])): Int = {

    def go(game: Int, round: Int, p1: Chain[Int], p2: Chain[Int], state: Set[(Chain[Int], Chain[Int])] = Set.empty): (Either[1,2], Chain[Int]) = {
      if (state.contains(p1 -> p2)) {
        val r: (Either[1, 2], Chain[Int]) = Left[1,2](1) -> p1
        r
      }
      else {
        val nextState = state + (p1 -> p2)
        (p1.uncons, p2.uncons) match {
          case (Some((p1h, p1t)), Some((p2h, p2t))) =>
            if (p1t.length >= p1h && p2t.length >= p2h) go(game + 1, 0, Chain.fromSeq(p1t.toList.take(p1h)), Chain.fromSeq(p2t.toList.take(p2h)), Set.empty) match {
              case (Left(1), _) => go(game, round + 1, p1t ++ Chain(p1h, p2h), p2t, nextState)
              case (Right(2), _) => go(game, round + 1, p1t, p2t ++ Chain(p2h, p1h), nextState)
            }
            else if (p1h > p2h) go(game, round + 1, p1t ++ Chain(p1h, p2h), p2t, nextState)
            else go(game, round + 1, p1t, p2t ++ Chain(p2h, p1h), nextState)
          case (None, Some(_)) => Right[1,2](2) -> p2
          case (Some(_), None) => Left[1,2](1) -> p1
          case (None, None) => throw new Exception("Inconceivable!")
        }
      }
    }

    val (p1, p2) = in
    val (_, stack) = go(0,0,p1, p2)
    val r = stack.reverse.zipWithIndex.map {
      case (el, i) => el * (i + 1)
    }

    r.toList.sum
  }

  val input = parse(Source.fromResource("day22/input.txt").getLines().toList)

  val t1Solution = task1(input)
  println(s"Task 1: $t1Solution")

  val t2Solution = task2(input)
  println(s"Task 2: $t2Solution")

}
