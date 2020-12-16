package jgogstad.day16

import cats.syntax.all._
import fs2.Stream
import org.jgrapht.alg.interfaces.MatchingAlgorithm.Matching
import org.jgrapht.alg.matching.HopcroftKarpMaximumCardinalityBipartiteMatching
import org.jgrapht.graph.{DefaultEdge, SimpleGraph}

import scala.io.Source
import scala.jdk.CollectionConverters._

object Tasks extends App {

  object intTuple {
    def unapply(s: String): Option[(Int, Int)] = {
      val a :: b :: Nil = s.split("-").toList
      Option(a.toInt -> b.toInt)
    }
  }

  val data = Source.fromResource("day16/input.txt").getLines().toList

  def parseInput(s: List[String]): (List[(String, Int => Boolean)], List[Int], List[List[Int]]) =
    Stream.emits(s).groupAdjacentBy(_.nonEmpty).mapFilter(t => t._1.guard[Option].as(t._2.toList)).compile.toList match {
      case rules :: ticket :: tickets :: Nil =>
        val r = rules.map {
          case s"$m: ${intTuple(a, b)} or ${intTuple(c, d)}" => (m, (i: Int) => Range.inclusive(a, b).contains(i) || Range.inclusive(c, d).contains(i))
          case _ => throw new Exception("Error")
        }
        val t = ticket.drop(1).flatMap(_.split(",").map(_.toInt).toList)
        val o = tickets.drop(1).map(_.split(",").map(_.toInt).toList)
        (r, t, o)
      case _ => throw new Exception("Invalid input")
    }

  def matching(rules: List[(String, Int => Boolean)], tickets: List[List[Int]]): Matching[String, DefaultEdge] = {
    val edges = rules.flatMap {
      case (s, rule) =>
        tickets
          .map(_.zipWithIndex.map(t => rule(t._1).guard[Option].as(t._2)).flattenOption)
          .reduce(_.intersect(_))
          .map(s -> _.toString)
    }

    val graph = new SimpleGraph[String, DefaultEdge](classOf[DefaultEdge])
    edges.foreach {
      case (s, t) =>
        graph.addVertex(s)
        graph.addVertex(t)
        graph.addEdge(s, t)
    }

    val (left, right) = edges.separate

    new HopcroftKarpMaximumCardinalityBipartiteMatching[String, DefaultEdge](
      graph,
      left.toSet.asJava,
      right.toSet.asJava
    ).getMatching
  }

  def solveTask1(rules: List[(String, Int => Boolean)], ticket: List[Int], tickets: List[List[Int]]): Int =
    tickets
      .map(t => t -> matching(rules, List(t)))
      .mapFilter(t => (!t._2.isPerfect).guard[Option].as(t._1))
      .map(_.map(t => rules.map(_._2.apply(t)).reduce(_ || _).guard[Option].fold(t)(_ => 0)).sum)
      .sum

  def solveTask2(rules: List[(String, Int => Boolean)], ticket: List[Int], tickets: List[List[Int]]): BigInt = {
    val validTickets = tickets.filter(t => matching(rules, List(t)).isPerfect)

    val m = matching(rules, validTickets)

    val graph = m.getGraph
    m.getEdges.asScala.toList
      .map(edge => graph.getEdgeSource(edge) -> graph.getEdgeTarget(edge))
      .mapFilter(t => t._1.startsWith("departure").guard[Option].as(t._2.toInt))
      .map(v => BigInt(ticket(v)))
      .product
  }

  val task1   = (parseInput _).andThen((solveTask1 _).tupled)
  val task2   = (parseInput _).andThen((solveTask2 _).tupled)

  println(s"Task1: ${task1(data)}")
  println(s"Task2: ${task2(data)}")
}
