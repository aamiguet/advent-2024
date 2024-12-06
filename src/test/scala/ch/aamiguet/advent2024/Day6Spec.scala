package ch.aamiguet.advent2024

import org.scalatest.*
import flatspec.*
import matchers.*

import scala.language.experimental.namedTuples

import Day6.*

class Day6Spec extends AnyFlatSpec with should.Matchers with Data("data/test6.txt"):

  given grid: Map[Coord, Tile] = parseGrid(lines)

  "Day6" should "correctly map the guard path" in {
    visitedTileCount() shouldBe 41
  }

  "Day6" should "fin all the potential blocking tiles" in {
    potentialLoopCount() shouldBe 6
  }
