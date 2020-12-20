package jgogstad.day19

import cats.syntax.all._

import scala.io.Source

object Tasks extends App {

  def parse(in: List[String]): (Map[Int, String], List[String]) = {
    val ruleRegex  = "(\\d+): ([\\d ]+(?:\\| [\\d ]+)?)$".r
    val matchRegex = "(\\d+): \"([a-z]+)\"".r

    val (ruleInput, data) = in.span(_.nonEmpty)

    val rules = ruleInput
      .map(s => matchRegex.findFirstMatchIn(s).orElse(ruleRegex.findFirstMatchIn(s)))
      .map {
        case Some(m) => m.group(1).toInt -> m.group(2)
        case None    => throw new Exception("Invalid input")
      }
      .toMap

    rules -> data.drop(1)
  }

  def run(rules: Map[Int, String], data: String, targetRule: Int = 0): Option[String] = {
    def go(curr: String, rule: String): Option[String] = {
      rule match {
        case s"$left | $right"                      => go(curr, left).orElse(go(curr, right))
        case single if single.toIntOption.isDefined => go(curr, rules(single.toInt))
        case all if all.contains(" ") =>
          all.split(" ").toList.map(_.toInt).foldLeft(curr.some) { (acc, el) =>
            acc match {
              case Some(s) => go(s, el.toString)
              case None    => None
            }
          }
        case single if single.length == 1 => if (curr.startsWith(single)) curr.tail.some else none
      }
    }

    go(data, rules(targetRule))
  }

  def task1(rules: Map[Int, String], data: List[String]): Int =
    data.map(s => run(rules, s, 0)).count(_.fold(false)(_.isEmpty))

  def task2(rules: Map[Int, String], data: List[String]): Int = {
    def applyRule8(result: List[String], s: String): List[String] =
      run(rules, s, 8) match {
        case Some(r) => applyRule8(r :: result, r)
        case None    => result
      }

    data.count(applyRule8(Nil, _).exists(run(rules, _, 11).fold(false)(_.isEmpty)))
  }

  val data      = Source.fromResource("day19/input.txt").getLines().toList
  val dataTask2 = Source.fromResource("day19/input_task2.txt").getLines().toList

  println(s"Task 1: ${(task1 _).tupled(parse(data))}")
  println(s"Task 2: ${(task2 _).tupled(parse(dataTask2))}")
}
