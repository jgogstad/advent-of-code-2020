package jgogstad.day23

import cats.syntax.all._

import scala.annotation.tailrec

object Tasks extends App {

  def game(in: String, cups: Int, rounds: Int): Array[Int] = {
    val start = in.split("").map(_.toInt)
    val rest  = (start.max + 1) to cups
    val succs = new Array[Int](cups + 1)

    fs2.Stream.emits(start ++ rest).zipWithNext.compile.toList.foreach {
      case (el, Some(next)) => succs.update(el, next)
      case (el, None)       => succs.update(el, start.head)
    }

    @tailrec
    def go(current: Int, round: Int): Unit = {
      val succ1     = succs(current)
      val succ3     = succs(succs(succ1))
      val succ4     = succs(succ3)
      val nextThree = Set(succ1, succs(succ1), succ3)

      @tailrec
      def dest(c: Int): Int = if (nextThree.contains(c)) dest(if (c - 1 < 1) cups else c - 1) else c
      val destination       = dest(if (current - 1 < 1) cups else current - 1)

      val succDestination = succs(destination)
      succs.update(destination, succ1)
      succs.update(succ3, succDestination)
      succs.update(current, succ4)

      if (round === rounds) ()
      else go(succs(current), round + 1)
    }

    go(start.head, 1)

    succs
  }

  def slice(succs: Array[Int], start: Int, n: Int): Array[Int] = {
    @tailrec
    def go(i: Int, el: Int, r: List[Int]): Array[Int] = if (i === n) r.reverse.toArray else go(i + 1, succs(el), el :: r)
    go(0, start, List.empty)
  }

  def task1(in: String): Array[Int] = {
    val result = game(in, in.length, 100)
    slice(result, result(1), in.length - 1)
  }

  def task2(in: String): BigInt = {
    val result     = game(in, 1000000, 10000000)
    val (one, two) = (result(1), result(result(1)))
    BigInt(one) * BigInt(two)
  }

  val input = "219347865"

  val task1Solution = task1(input).mkString("")
  println(s"Task 1: $task1Solution")

  val task2Solution = task2(input)
  println(s"Task 2: $task2Solution")

}
