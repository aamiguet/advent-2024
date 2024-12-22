package ch.aamiguet.advent2024

import org.scalatest.*
import flatspec.*
import matchers.*

import Day22.*

class Day22Spec extends AnyFlatSpec with should.Matchers with Data("data/test22.txt"):

  lazy val monkeys = parse(lines)

  val lines2 =
    """1
      |2
      |3
      |2024""".stripMargin.split("\n").toList
  lazy val monkeys2 = parse(lines2)

  "Day22" should "simulate the create of secrets" in {
    sumOfLastSecret(monkeys).shouldBe(37327623L)
  }

  "Day22" should "find the best sequence and the number of bananas" in {
    mostBananas(monkeys2).shouldBe(23L)
  }
