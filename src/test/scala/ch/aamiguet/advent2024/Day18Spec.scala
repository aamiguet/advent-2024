package ch.aamiguet.advent2024

import scala.language.experimental.namedTuples

import org.scalatest.*
import flatspec.*
import matchers.*

import Day18.*

class Day18Spec extends AnyFlatSpec with should.Matchers with Data("data/test18.txt"):

  given memory: MemorySpace = parse(lines, 6, 6)

  "Day18" should "compute the shortest path" in {
    part1(12).head.shouldBe(22)
  }

  "Day18" should "find the first byte blocking the exit path" in {
    part2(12).shouldBe((6, 1))
  }
