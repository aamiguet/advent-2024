package ch.aamiguet.advent2024

import org.scalatest.*
import flatspec.*
import matchers.*

import Day25.*

class Day25Spec extends AnyFlatSpec with should.Matchers with Data("data/test25.txt"):

  val (locks, keys) = parse(lines)

  "Day25" should "find all fitting pairs of key and lock" in {
    fitCount(locks, keys).shouldBe(3)
  }
