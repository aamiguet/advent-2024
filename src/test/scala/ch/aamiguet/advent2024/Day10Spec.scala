package ch.aamiguet.advent2024

import scala.language.experimental.namedTuples

import org.scalatest.*
import flatspec.*
import matchers.*

import Day10.*

class Day10Spec extends AnyFlatSpec with should.Matchers with Data("data/test10.txt"):

  given map: Map[Position, Int] = parse(lines)

  "Day10" should "compute the trail score" in {
    totalScore() shouldBe 36
    totalScoreAlt() shouldBe 81
  }
