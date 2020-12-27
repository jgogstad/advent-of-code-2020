package jgogstad.day21

import org.jgrapht.graph.{DefaultEdge, SimpleGraph}
import io.estatico.newtype.macros.newtype
import cats.syntax.all._
import org.jgrapht.alg.interfaces.MatchingAlgorithm
import org.jgrapht.alg.matching.HopcroftKarpMaximumCardinalityBipartiteMatching

import scala.io.Source
import scala.jdk.CollectionConverters._

object Tasks extends App {

  val example = """mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
                  |trh fvjkl sbzzf mxmxvkd (contains dairy)
                  |sqjhc fvjkl (contains soy)
                  |sqjhc mxmxvkd sbzzf (contains fish)""".stripMargin.split("\n").toList

  type Ingredient = String
  type Allergen   = String

  def parseInput(in: List[String]): List[(Set[Ingredient], Set[Allergen])] =
    in.map("([^\\(]+) \\(contains (.*)\\)".r.findFirstMatchIn)
      .map {
        case Some(m) => m.group(1).split(" ").toSet -> m.group(2).split(" ").map(_.stripSuffix(",")).toSet
      }

  def findMatching(in: List[(Set[Ingredient], Set[Allergen])]): MatchingAlgorithm.Matching[String, DefaultEdge] = {
    val graph = new SimpleGraph[String, DefaultEdge](classOf[DefaultEdge])

    val edges: List[(Allergen, Set[Ingredient])] = in
      .flatMap {
        case (is, as) => as.toList.map(_ -> is)
      }
      .groupMap(_._1)(_._2)
      .map {
        case (a, is) => a -> is.reduce(_ intersect _)
      }
      .toList

    edges.foreach {
      case (a, is) =>
        graph.addVertex(a)
        is.foreach { i =>
          graph.addVertex(i)
          graph.addEdge(a, i)
        }
    }

    val (left, right) = edges.separate
    new HopcroftKarpMaximumCardinalityBipartiteMatching[String, DefaultEdge](
      graph,
      left.toSet.asJava,
      right.flatMap(_.toList).toSet.asJava
    ).getMatching
  }

  def task1(in: List[(Set[Ingredient], Set[Allergen])]): Int = {
    val matching    = findMatching(in)
    val ingredients = in.flatMap(_._1.toList)
    val missing     = ingredients.toSet.diff(matching.getEdges.asScala.map(matching.getGraph.getEdgeTarget))

    ingredients.count(missing.contains)
  }

  def task2(in: List[(Set[Ingredient], Set[Allergen])]): String = {
    val m        = findMatching(in)
    val mappings = m.getEdges.asScala.map(e => m.getGraph.getEdgeSource(e) -> m.getGraph.getEdgeTarget(e))
    mappings.toList.sortBy(_._1).map(_._2).mkString(",")
  }

  val input         = parseInput(Source.fromResource("day21/input.txt").getLines().toList)
  val task1Solution = task1(input)
  val task2Solution = task2(input)

  println(s"Task 1: $task1Solution")
  println(s"Task 2: $task2Solution")
}
