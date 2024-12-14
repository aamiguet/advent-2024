package ch.aamiguet.advent2024

import scala.language.experimental.namedTuples

import org.scalatest.*
import flatspec.*
import matchers.*

import Day14.*

class Day14Spec extends AnyFlatSpec with should.Matchers with Data("data/test14.txt"):

  val securitySpace = parse(lines, 11, 7)

  "Day14" should "compute the safety factor" in {
    securitySpace.safetyFactor(100) shouldBe 12
  }
