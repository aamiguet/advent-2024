package ch.aamiguet.advent2024

import org.scalatest.*
import flatspec.*
import matchers.*

import Day19.*

class Day19Spec extends AnyFlatSpec with should.Matchers with Data("data/test19.txt"):

  val onsen = Onsen(lines)

  "Day19" should "identify the number of possible designs" in {
    part1(onsen).shouldBe(6)
  }

  "Day19" should "compute the number posssible arrangements of a design" in {
    onsen.arrangementNumber("brwrr").shouldBe(2L)
    onsen.arrangementNumber("bggr").shouldBe(1L)
    onsen.arrangementNumber("gbbr").shouldBe(4L)
    onsen.arrangementNumber("rrbgbr").shouldBe(6L)
    onsen.arrangementNumber("bwurrg").shouldBe(1L)
    onsen.arrangementNumber("brgr").shouldBe(2L)
  }

  "Day19" should "compute the total number of possible arrangements" in {
    part2(onsen).shouldBe(16L)
  }
