package ch.aamiguet.advent2024

import org.scalatest.*
import flatspec.*
import matchers.*
import scala.language.experimental.namedTuples

import Day4.*

class Day4Spec extends AnyFlatSpec with should.Matchers:

  val test =
    """MMMSXXMASM
      |MSAMXMSMSA
      |AMXSXMAAMM
      |MSAMASMSMX
      |XMASAMXAMM
      |XXAMMXXAMA
      |SMSMSASXSS
      |SAXAMASAAA
      |MAMMMXMMMM
      |MXMXAXMASX""".stripMargin

  val grid = gridMap(test.split("\n").toList)

  "Day4" should "find the 18 XMAS words" in {
    wordCount(grid) shouldBe 18
  }

  "Day4" should "find the 9 X-MAS crosses" in {
    crossCount(grid) shouldBe 9
  }
