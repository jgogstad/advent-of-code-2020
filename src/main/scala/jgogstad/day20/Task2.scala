package jgogstad.day20

import breeze.linalg.{sum, Axis, DenseMatrix}
import cats.syntax.all._
import breeze.linalg.DenseMatrix.FrobeniusInnerProductDenseMatrixSpace

import scala.io.Source
import scala.util.Try

object Task2 extends App {

  // Brute force solve puzzle from upper left corner
  def solvePuzzle(ps: List[Piece]): Option[DenseMatrix[Piece]] = {
    val permutations = ps.flatMap(Task1.permutations).toSet
    val adjacent     = Task1.createAdjacencyMap(permutations)

    def solve(result: DenseMatrix[Option[Piece]], i: Int, j: Int, remaining: Set[Piece]): Option[DenseMatrix[Piece]] = {
      val up   = Try(result(i - 1, j)).toOption.flatten
      val left = Try(result(i, j - 1)).toOption.flatten

      val nextPieceCandidates = {
        (up, left) match {
          case (None, Some(l))    => adjacent(l)._2.filter(remaining.contains)
          case (Some(u), None)    => adjacent(u)._3.filter(remaining.contains)
          case (Some(u), Some(l)) => adjacent(u)._3.intersect(adjacent(l)._2).filter(remaining.contains)
          case (None, None) => Nil
        }
      }
      nextPieceCandidates match {
        case list@_ :: _ =>
          val (ni, nj) = if (j + 1 < result.cols) i -> (j + 1) else (i + 1) -> 0

          list.toList.collectFirstSome { h =>
            val r = result.copy
            r.update(i, j, h.some)
            solve(r, ni, nj, remaining.filter(_.number =!= h.number))
          }
        case Nil => Try(result.map(_.get)).toOption // missing Traverse instance ☹️
      }
    }

    val upperLeftCorner = permutations.find { p =>
      adjacent(p) match {
        case (Nil, _ :: _, _ :: _, Nil) => true
        case _                          => false
      }
    }

    upperLeftCorner.flatMap { corner =>
      val dimension = Math.sqrt(ps.length.toDouble).toInt
      val init      = DenseMatrix.fill(dimension, dimension)(none[Piece])
      init.update(0, 0, corner.some)

      solve(init, 0, 1, permutations.filter(_.number =!= corner.number))
    }
  }

  def task2(seaMonster: DenseMatrix[Int])(input: List[Piece]): Option[Int] = {

    def stripEdges(puzzle: DenseMatrix[Piece]): DenseMatrix[Int] = {
      val ms: DenseMatrix[DenseMatrix[Int]] = puzzle.map(_.data)
      val catColumns                        = (0 until puzzle.rows).map(i => DenseMatrix.horzcat(ms(i, ::).inner.toArray.toList: _*))

      val deletions = (1 until ms.rows).foldLeft(List.empty[Int]) { (acc, el) =>
        val right = el * 10
        (right :: (right - 1) :: Nil) ::: acc
      } ++ List(0, DenseMatrix.vertcat(catColumns: _*).rows - 1)

      DenseMatrix.vertcat(catColumns: _*).delete(deletions, Axis._0).delete(deletions, Axis._1)
    }

    solvePuzzle(input).flatMap { puzzle =>
      val puzzleContents = stripEdges(puzzle)

      val piece = Piece(0, puzzleContents)
      val ms = Task1.permutations(piece).map { p =>
        val seaMonsterSize      = sum(seaMonster)
        val seaMonsterLocations = convolve(seaMonster, p.data).map(i => if (i === seaMonsterSize) 1 else 0)
        val seaMonsters         = sum(seaMonsterLocations)
        (seaMonsters > 0).guard[Option].as(sum(p.data) - (seaMonsters * seaMonsterSize))
      }
      ms.toList.flattenOption.headOption
    }
  }

  /**
    * Naïve matrix convolution.
    *
    * Breeze doesn't have matrix convolution according to https://github.com/scalanlp/breeze/blob/master/math/src/main/scala/breeze/signal/support/CanConvolve.scala#L17
    */
  def convolve(kernel: DenseMatrix[Int], data: DenseMatrix[Int]): DenseMatrix[Int] =
    data.mapPairs {
      case ((i, j), _) =>
        Try(data(i until (i + kernel.rows), j until (j + kernel.cols)))
          .map(slice => sum(FrobeniusInnerProductDenseMatrixSpace.space[Int].mulVV(kernel, slice)))
          .toOption
          .getOrElse(0)
    }

  val kernel: DenseMatrix[Int] = {
    val data = Source.fromResource("day20/seamonster.txt").getLines().toArray
    val max  = data.map(_.length).max
    DenseMatrix(data.toList.map(_.padTo(max, " ").mkString.toList): _*).map(c => if (c === '#') 1 else 0)
  }

  val solution = Task1.parse("input.txt").map(task2(kernel))

  println(s"Task 2: $solution")
}
