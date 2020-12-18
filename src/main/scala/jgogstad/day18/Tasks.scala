package jgogstad.day18

import fastparse._
import fastparse.SingleLineWhitespace._

import scala.io.Source

object Tasks extends App {

  private val data = Source.fromResource("day18/input.txt").getLines().toList

  def eval(tree: (BigInt, Seq[(String, BigInt)])) = {
    val (base, ops) = tree
    ops.foldLeft(base) {
      case (left, (op, right)) =>
        op match {
          case "+" => left + right
          case "*" => left * right
        }
    }
  }

  def number[_: P]: P[BigInt] = P(CharIn("0-9").rep(1).!).map(BigInt.apply)

  def task1[_: P]: P[BigInt] = {
    def parens: P[BigInt] = P("(" ~ addMul ~ ")")
    def factor: P[BigInt] = P(number | parens)
    def addMul: P[BigInt] = P(factor ~ (CharIn("*+").! ~/ factor).rep).map(eval)

    P(addMul ~ End)
  }

  def task2[_: P]: P[BigInt] = {
    def parens: P[BigInt] = P("(" ~ mul ~ ")")
    def factor: P[BigInt] = P(number | parens)
    def add: P[BigInt]    = P(factor ~ (CharIn("+").! ~/ factor).rep).map(eval)
    def mul: P[BigInt]    = P(add ~ (CharIn("*").! ~/ add).rep).map(eval)
    P(mul ~ End)
  }

  val t1 = data.map(parse(_, task1(_)).get.value).sum
  val t2 = data.map(parse(_, task2(_)).get.value).sum

  println(s"Task 1: $t1")
  println(s"Task 1: $t2")
}
