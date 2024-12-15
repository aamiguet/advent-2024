package ch.aamiguet.advent2024

import scala.language.experimental.namedTuples

import org.scalatest.*
import flatspec.*
import matchers.*

import Day15.*

class Day15Spec extends AnyFlatSpec with should.Matchers with Data("data/test15.txt"):

  val lines2 =
    """#######
      |#...O..
      |#......""".stripMargin.split("\n").toList

  val lines3 =
    """#######
      |#...#.#
      |#.....#
      |#..OO@#
      |#..O..#
      |#.....#
      |#######
      |
      |<vv<<^^<<^^""".stripMargin.split("\n").toList

  "Day15" should "compute the sum of the sample" in {
    val (grid, moves) = parse(lines2)
    gpsSum(grid) shouldBe 104
  }

  "Day15" should "compute the sum of GPS corrdinates" in {
    part1(lines) shouldBe 10092
  }

  "Day15" should "compute the sum of GPS corrdinates in the larger warehouse" in {
    part2(lines3) shouldBe 618
    part2(lines) shouldBe 9021
  }
