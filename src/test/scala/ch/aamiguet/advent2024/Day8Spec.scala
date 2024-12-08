package ch.aamiguet.advent2024

import scala.language.experimental.namedTuples

import org.scalatest.*
import flatspec.*
import matchers.*

import Day8.*

class Day8Spec extends AnyFlatSpec with should.Matchers with Data("data/test8.txt"):

  given city: City = City(lines)

  "Day8" should "identify all the antinodes locations" in {
    oppositeAntinodesPosition().size shouldBe 14
    resonantAntinodePosition().size shouldBe 34
  }
