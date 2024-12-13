package ch.aamiguet.advent2024

import org.scalatest.*
import flatspec.*
import matchers.*

import Day13.*

class Day13Spec extends AnyFlatSpec with should.Matchers with Data("data/test13.txt"):

  val cws = parse(lines)

  "Day13" should "compute the fewest tokens" in {
    fewestTokens(cws) shouldBe 480L
  }
