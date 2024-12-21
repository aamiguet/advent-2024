package ch.aamiguet.advent2024

import scala.language.experimental.namedTuples

import org.scalatest.*
import flatspec.*
import matchers.*

import Day21.*

class Day21Spec extends AnyFlatSpec with should.Matchers with Data("data/test21.txt"):

  "Day21" should "compute the complexity" in {
    part1(lines).shouldBe(126384)
  }
