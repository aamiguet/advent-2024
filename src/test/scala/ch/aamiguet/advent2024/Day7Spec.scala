package ch.aamiguet.advent2024

import org.scalatest.*
import flatspec.*
import matchers.*

import Day7.*

class Day7Spec extends AnyFlatSpec with should.Matchers with Data("data/test7.txt"):

  val equations = parseEquations(lines)

  "Day7" should "identify the correct equations and produce the total calibration result" in {
    totalCalibrationResult(equations) shouldBe 3749L
    totalCalibrationResultWithConcat(equations) shouldBe 11387L
  }
