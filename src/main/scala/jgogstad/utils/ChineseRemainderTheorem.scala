package jgogstad.utils

import cats.syntax.all._

import scala.annotation.tailrec

object ChineseRemainderTheorem {

  def solve(congruences: List[(Int, Int)]): Option[BigInt] = {
    val (coeffs, mods) = congruences.map(t => BigInt(t._1) -> BigInt(t._2)).separate

    isCoPrime(mods).guard[Option].as {
      val modsProduct = mods.product
      val zs = mods.map(modsProduct / _)
      val zInvs = zs.zip(mods).map((multiplicativeInverse _).tupled)

      val s = zs.zip(zInvs).zip(coeffs).foldMap {
        case ((c, i), m) => c * i * m
      }

      s % modsProduct
    }
  }

  private def isCoPrime(ints: List[BigInt]): Boolean = {
    ints.zip(ints.tails.toList.tail).traverse_ {
      case (i, t) => t.map(gcd(i)).traverse_(g => (g === 1).guard[Option])
    }
  }.isDefined

  def gcd(a: BigInt)(b: BigInt): BigInt = {
    if(b ==0) a else gcd(b)(a % b)
  }

  private def multiplicativeInverse(a: BigInt, mod: BigInt): BigInt = {
    @tailrec
    def loop(a: BigInt, b: BigInt, x0: BigInt, x1: BigInt): BigInt = {
      if (a > 1) loop(b, a % b, x1 - (a / b) * x0, x0) else x1
    }

    if (mod == 1) 1
    else {
      val x1 = loop(a, mod, 0, 1)
      if (x1 < 0) x1 + mod else x1
    }
  }
}
