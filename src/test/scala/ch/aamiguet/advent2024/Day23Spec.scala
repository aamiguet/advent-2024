package ch.aamiguet.advent2024

import org.scalatest.*
import flatspec.*
import matchers.*

import Day23.*

class Day23Spec extends AnyFlatSpec with should.Matchers with Data("data/test23.txt"):

  val compMap = parse(lines)

  "Day23" should "find all triplets with a computer starting with t" in {
    part1(compMap).shouldBe(7)
  }

  "Day23" should "find the lan password" in {
    lanPassword(compMap).shouldBe("co,de,ka,ta")
  }
