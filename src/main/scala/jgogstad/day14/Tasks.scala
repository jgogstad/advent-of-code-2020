package jgogstad.day14

import cats.data.NonEmptyList
import cats.syntax.all._

import scala.annotation.tailrec
import scala.io.Source
import scala.util.{Failure, Success, Try}

object Tasks extends App {

  object mem { def unapply(s: String): Option[Long] = "mem\\[(\\d+)\\]".r.findFirstMatchIn(s).flatMap(m => Try(m.group(1).toLong).toOption) }

  object long { def unapply(s: String): Option[Long] = s.toLongOption }

  val data = Source.fromResource("day14/input.txt").getLines().toList

  def solve(input: List[String])(f: (String, Map[Long, Long], Long, Long) => Map[Long, Long]): Try[Long] = {
    val result = input.foldLeft(Try(("", Map.empty[Long, Long]))) {
      case (Success((mask, memory)), el) =>
        el match {
          case s"mask = $m"              => Success(m -> memory)
          case s"${mem(i)} = ${long(v)}" => Success(mask -> f(mask, memory, i, v))
          case s                         => Failure(new Exception(show"invalid input $s"))
        }
      case (acc, _) => acc
    }
    result.map(_._2.values.sum)
  }

  def task1(mask: String, value: Long): Long = {
    val (zeros, ones) = mask.reverse.toList.zipWithIndex.filter(_._1 != 'X').partition(_._1 === '0')
    val setZeros      = NonEmptyList.fromList(zeros).map(_.map(_._2).map(i => ~(1L << i)).reduce((_: Long) & (_: Long))).getOrElse(Long.MaxValue)
    val setOnes       = NonEmptyList.fromList(ones).map(_.map(_._2).map(i => 1L << i).reduce((_: Long) | (_: Long))).getOrElse(0L)

    setZeros & value | setOnes
  }

  def task2(mask: String, memory: Map[Long, Long], address: Long, value: Long): Map[Long, Long] = {
    @tailrec
    def calcMasks(result: List[String], stack: List[String]): List[String] = stack match {
      case Nil => result
      case h :: t =>
        h.indexOf('X') match {
          case i if i < 0 => calcMasks(h :: result, t)
          case _          => calcMasks(result, h.replaceFirst("X", "0") :: h.replaceFirst("X", "1") :: t)
        }
    }

    val addresses = calcMasks(List.empty, List(mask)).map { m =>
      val newMask = m.toList.zipWithIndex
        .map {
          case (c, i) if c == '0' && mask.charAt(i) == 'X' => '0'
          case (c, _) if c == '1'                          => '1'
          case _                                           => 'X'
        }
        .mkString("")
      task1(newMask, address)
    }
    addresses.foldLeft(memory)((acc, el) => acc + (el -> value))
  }

  def applyTask1(mask: String, memory: Map[Long, Long], address: Long, value: Long): Map[Long, Long] =
    memory + (address -> task1(mask, value))

  println(s"task1: ${solve(data)(applyTask1)}")
  println(s"task2: ${solve(data)(task2)}")
}
