package jgogstad.day25

import cats.syntax.all._

object Tasks extends App {

  val (door, card) = (16616892, 14505727)
  val (exampleDoor, exampleCard) = (17807724, 5764801)

  def findLoopSize(publicKey: Int, subjectNumber: Int): Int = {
    def go(current: BigInt, loop: Int): Int =
      if (current === publicKey) loop
      else go((current * subjectNumber) % 20201227, loop + 1)

    go(subjectNumber, 1)
  }

  def encryptionKey(loopSize: Int, subjectNumber: Int): Int = {
    def go(i: Int, v: Int): Int =
      if (i >= loopSize) v
      else go(i + 1, ((BigInt(v) * subjectNumber) % 20201227).toInt)

    go(0, 1)
  }

  val doorLoop = findLoopSize(door, 7)
  val cardLoop = findLoopSize(card, 7)

  println(encryptionKey(doorLoop, card))
  println(encryptionKey(cardLoop, door))


}
