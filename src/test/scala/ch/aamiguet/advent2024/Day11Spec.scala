package ch.aamiguet.advent2024

import org.scalatest.*
import flatspec.*
import matchers.*

import Day11.*

class Day11Spec extends AnyFlatSpec with should.Matchers with Data("data/test11.txt"):

  "Day11" should "find the number of stones" in {
    stoneCount(lines.head, 25) shouldBe 55312L
  }
