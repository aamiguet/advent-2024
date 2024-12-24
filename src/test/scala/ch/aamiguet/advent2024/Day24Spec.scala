package ch.aamiguet.advent2024

import org.scalatest.*
import flatspec.*
import matchers.*

import Day24.*

class Day24Spec extends AnyFlatSpec with should.Matchers with Data("data/test24.txt"):

  lazy val (state, gates) = parse(lines)

  "Day24" should "compute the system value" in {
    part1(state, gates).shouldBe(2024)
  }
