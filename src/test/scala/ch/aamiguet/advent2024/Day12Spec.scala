package ch.aamiguet.advent2024

import scala.language.experimental.namedTuples

import org.scalatest.*
import flatspec.*
import matchers.*

import Day12.*

class Day12Spec extends AnyFlatSpec with should.Matchers with Data("data/test12.txt"):

  given farmMap: Map[Position, Char] = parse(lines)

  "Day12" should "compute the total price" in {
    totalPrice() shouldBe 1930L
    discountedTotalPrice() shouldBe 1206L
  }
