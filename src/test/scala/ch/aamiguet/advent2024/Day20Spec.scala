package ch.aamiguet.advent2024

import scala.language.experimental.namedTuples

import org.scalatest.*
import flatspec.*
import matchers.*

import Day20.*

class Day20Spec extends AnyFlatSpec with should.Matchers with Data("data/test20.txt"):

  val racetrack = Racetrack(lines)

  "Day20" should "find all the possible cheats" in {
    racetrack.possibleCheats(1, 2).shouldBe(44)
    racetrack.possibleCheats(50, 20).shouldBe(285)
  }
