package ch.aamiguet.advent2024

import org.scalatest.*
import flatspec.*
import matchers.*

import Day1.*

class Day1Spec extends AnyFlatSpec with should.Matchers:

  "Day1" should "compute the sum" in {
    sum(1, 2) shouldBe 3
  }
