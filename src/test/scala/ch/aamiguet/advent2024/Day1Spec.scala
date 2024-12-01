package ch.aamiguet.advent2024

import org.scalatest.*
import flatspec.*
import matchers.*

import Day1.*

class Day1Spec extends AnyFlatSpec with should.Matchers:

  val test =
    """3   4
      |4   3
      |2   5
      |1   3
      |3   9
      |3   3""".stripMargin
  val (left, right) = lists(test.split("\n").toList)

  "Day1" should "compute the total distance" in {
    distance(left, right) shouldBe 11
  }

  "Day1" should "compute the similarity" in {
    similarity(left, right) shouldBe 31
  }
