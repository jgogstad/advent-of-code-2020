package jgogstad.day7

import cats.effect.{Blocker, ExitCode, IO, IOApp}
import fs2.Stream
import cats.syntax.all._
import io.odin.{consoleLogger, Logger}
import jgogstad.utils.FileIo

import scala.annotation.tailrec
import scala.util.{Failure, Try}

object Tasks extends IOApp {
  private val log: Logger[IO] = consoleLogger()

  private def parse(line: String): Try[(String, List[(Int, String)])] =
    line.split(" bags contain ").toList match {
      case h :: t :: Nil =>
        val regex = "(\\d)+ ([\\w ]+?) bags?".r
        val list  = regex.findAllMatchIn(t).toList
        list.traverse(m => (Try(m.group(1).toInt), Try(m.group(2))).tupled).map(h -> _)
      case _ => Failure(new Exception(show"Unexpected format: $line"))
    }

  private def solveTask1(data: Map[String, List[String]]): Int = {
    @tailrec
    def go(acc: Set[String], q: List[String]): Set[String] =
      q match {
        case Nil => acc
        case h :: t =>
          data.get(h) match {
            case Some(vs) => go(acc + h, t ::: vs)
            case None     => go(acc + h, t)
          }
      }
    go(Set.empty, List("shiny gold")).size - 1
  }

  private def solveTask2(target: String)(data: Map[String, List[(Int, String)]]): Option[Int] = {
    @tailrec
    def go(memo: Map[String, Int], stack: List[(String, List[(Int, String)])]): Option[Int] =
      stack match {
        case Nil            => memo.get(target)
        case (el, Nil) :: t => go(memo + (el -> 0), t)
        case (el, neighbours) :: t =>
          val nsum = neighbours.traverse { case (n, name) => memo.get(name).map(_ * n + n) }.map(_.sum)
          nsum match {
            case Some(sum) => go(memo + (el -> sum), t)
            case None =>
              val next = neighbours
                .map(_._2)
                .filter(t => !memo.contains(t))
                .map(n => n -> data.getOrElse(n, List.empty[(Int, String)]))

              go(memo, next ::: stack)
          }
      }
    data.get(target).flatMap(n => go(Map.empty, List(target -> n)))
  }

  override def run(args: List[String]): IO[ExitCode] = Blocker[IO].use { blocker =>
//    val task1 = exampleData
    val task1 = FileIo
      .contentsOf[IO]("day7/input.txt", blocker)
      .evalMap(parse _ andThen (_.liftTo[IO]))
      .fold(Map.empty[String, List[String]]) {
        case (acc, (container, contained)) =>
          contained.foldLeft(acc) { (a, e) =>
            val v = a.getOrElse(e._2, List.empty[String])
            a + (e._2 -> (container :: v))
          }
      }
      .map(solveTask1)
      .compile
      .lastOrError

    val task2 = FileIo.contentsOf[IO]("day7/input.txt", blocker)
      .evalMap(parse _ andThen (_.liftTo[IO]))
      .foldMap { case (k, v) => Map(k -> v) }
      .map(solveTask2("shiny gold"))
      .unNone
      .compile
      .lastOrError

    (task1, task2).parTupled
      .flatTap {
        case (r1, r2) => log.info(show"Task1: $r1") >> log.info(show"Task2: $r2")
      }.as(ExitCode.Success)
  }

  private val exampleData: Stream[IO, String] = {
    val data = """light red bags contain 1 bright white bag, 2 muted yellow bags.
                 |dark orange bags contain 3 bright white bags, 4 muted yellow bags.
                 |bright white bags contain 1 shiny gold bag.
                 |muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
                 |shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
                 |dark olive bags contain 3 faded blue bags, 4 dotted black bags.
                 |vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
                 |faded blue bags contain no other bags.
                 |dotted black bags contain no other bags.""".stripMargin
    Stream.emits(data.split("\n")).covary[IO]
  }
}
