package ch.aamiguet.advent2024

import scala.language.experimental.namedTuples

import org.scalatest.*
import flatspec.*
import matchers.*

import Day16.*

class Day16Spec extends AnyFlatSpec with should.Matchers with Data("data/test16.txt"):

  val lines2 =
    """#################
      |#...#...#...#..E#
      |#.#.#.#.#.#.#.#.#
      |#.#.#.#...#...#.#
      |#.#.#.#.###.#.#.#
      |#...#.#.#.....#.#
      |#.#.#.#.#.#####.#
      |#.#...#.#.#.....#
      |#.#.#####.#.###.#
      |#.#.#.......#...#
      |#.#.###.#####.###
      |#.#.#...#.....#.#
      |#.#.#.#####.###.#
      |#.#.#.........#.#
      |#.#.#.#########.#
      |#S#.............#
      |#################""".stripMargin.split("\n").toList
  val grid2 = parse(lines2)

  given maze: Map[Position, Tile] = parse(lines)

  "Day16" should "compute the lowest score" in {
    lowestScore()(using grid2) shouldBe 11048
    lowestScore() shouldBe 7036
  }

  "Day16" should "find the best tiles" in {
    bestTiles()(using grid2) shouldBe 64
    bestTiles() shouldBe 45
  }
