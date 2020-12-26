package jgogstad.day20
import breeze.linalg._
import cats.syntax.all._
import fs2.Fallible

import scala.io.Source
import scala.util.Try

object Task1 extends App {

  def parse(fileName: String): Try[List[Piece]] =
    fs2.Stream
      .emits(Source.fromResource(s"day20/$fileName").getLines().toList)
      .covary[Fallible]
      .groupAdjacentBy(_.nonEmpty)
      .mapFilter(t => t._1.guard[Option].as(t._2.toList))
      .map {
        case h :: t =>
          Try {
            val number = "Tile (\\d+):".r.findFirstMatchIn(h).map(m => m.group(1).toInt).get
            val matrix = DenseMatrix(t.toArray.map(_.toCharArray.map(c => if (c === '#') 1 else 0)).toList: _*)

            Piece(number, matrix)
          }.toEither
        case _ => new Exception("Invalid input").raiseError[Either[Throwable, *], Piece]
      }
      .rethrow
      .compile
      .toList
      .toTry

  /**
    * @return Function that takes a piece and returns a tuple of adjacent pieces in order: (up, right, down, left)
    */
  def createAdjacencyMap(nodes: Set[Piece]): Piece => (List[Piece], List[Piece], List[Piece], List[Piece]) = {
    val (up, right, down, left) = nodes.foldLeft(
      (
        Map.empty[Transpose[DenseVector[Int]], List[Piece]],
        Map.empty[DenseVector[Int], List[Piece]],
        Map.empty[Transpose[DenseVector[Int]], List[Piece]],
        Map.empty[DenseVector[Int], List[Piece]]
      )
    ) {
      case ((up, right, down, left), el) =>
        (
          up.updatedWith(el.data(0, ::))(_.map(el :: _).orElse(List(el).some)),
          right.updatedWith(el.data(::, -1))(_.map(el :: _).orElse(List(el).some)),
          down.updatedWith(el.data(-1, ::))(_.map(el :: _).orElse(List(el).some)),
          left.updatedWith(el.data(::, 0))(_.map(el :: _).orElse(List(el).some))
        )
    }

    p => {
      (
        down.get(p.data(0, ::)).map(_.filter(_.number =!= p.number)).getOrElse(Nil),
        left.get(p.data(::, -1)).map(_.filter(_.number =!= p.number)).getOrElse(Nil),
        up.get(p.data(-1, ::)).map(_.filter(_.number =!= p.number)).getOrElse(Nil),
        right.get(p.data(::, 0)).map(_.filter(_.number =!= p.number)).getOrElse(Nil)
      )
    }
  }

  def permutations(p: Piece): Set[Piece] = {
    def rotations(l: Piece, r: Set[Piece] = Set.empty): Set[Piece] =
      if (r.contains(l)) r
      else rotations(l.copy(data = fliplr(l.data.t)), r + l)

    rotations(p) ++ rotations(p.copy(data = fliplr(p.data)))
  }

  def task1(ps: List[Piece]): BigInt = {
    val all      = ps.flatMap(permutations).toSet
    val adjacent = createAdjacencyMap(all)

    all
      .map { p =>
        adjacent(p) match { // Find corners
          case (_ :: _, _ :: _, Nil, Nil) => p.some
          case (Nil, _ :: _, _ :: _, Nil) => p.some
          case _                          => None
        }
      }
      .toList
      .flattenOption
      .map(p => BigInt(p.number))
      .distinct
      .product
  }

  val solution = parse("example.txt").map(task1)

  println(s"Task 1: ${solution.get}")
}
