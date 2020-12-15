package jgogstad.day15

import cats.data.NonEmptyList
import cats.syntax.all._

import scala.annotation.tailrec

object Tasks extends App {

  def solve(start: NonEmptyList[Int], limit: Int): Int = {
    @tailrec
    def go(iteration: Int, last: Int, memory: Map[Int, (Int, Option[Int])]): Int =
      if (iteration === limit + 1) last
      else {
        val v = memory.get(last) match {
          case Some((t1, Some(t2)))   => t1 - t2
          case Some((_, None)) | None => 0
        }
        go(iteration + 1, v, memory + (v -> (iteration -> memory.get(v).map(_._1.some).getOrElse(none[Int]))))
      }

    val memory = Map(start.toList.zipWithIndex.map(t => (t._1, (t._2 + 1, none[Int]))): _*)
    go(start.length + 1, start.last, memory)
  }

  val data  = "20,9,11,0,1,2"
  val start = NonEmptyList.fromListUnsafe(data.split(",").toList.map(_.toInt))

  println(s"task1: ${solve(start, 2020)}")
  println(s"task2: ${solve(start, 30000000)}")
}
