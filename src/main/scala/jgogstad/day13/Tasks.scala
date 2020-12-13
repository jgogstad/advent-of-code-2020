package jgogstad.day13

import algebra.ring.{CommutativeRing, MultiplicativeGroup, MultiplicativeSemigroup}

import scala.io.Source
import cats.syntax.all._
import spire.implicits._
import jgogstad.utils.ChineseRemainderTheorem

object Tasks extends App {

  def readInput(file: String): (Int, List[(Int, Int)]) = Source.fromResource(s"day13/$file").getLines().toList match {
    case h :: t :: Nil => h.toInt -> t.split(",").toList.zipWithIndex.mapFilter(t => t._1.toIntOption.map(_ -> t._2))
    case _             => throw new Exception("Invalid input")
  }

  val task1 = {
    val (dep, buses) = readInput("task1.txt")
    val (bus, wait)  = buses.map(_._1).map(bus => bus -> ((dep - (dep % bus) + bus) - dep)).minBy(_._2)
    bus * wait
  }

  val task2 = {
    val (_, buses) = readInput("task2.txt")
    ChineseRemainderTheorem.solve(buses.map(t => (t._1 - t._2, t._1)))
  }

  println(s"task1: $task1")
  println(s"task2: $task2")

}
