package ch.aamiguet.advent2024

import org.scalatest.*
import flatspec.*
import matchers.*

import Day9.*

class Day9Spec extends AnyFlatSpec with should.Matchers with Data("data/test9.txt"):

  "Day9" should "compute the checksum" in {
    val disk = parse(lines.mkString)._1
    compact(disk)
    checksum(disk) shouldBe 1928L
  }

  "Day9" should "compute the fragmented checksum" in {
    val (disk, fm) = parse(lines.mkString)
    compactWithoutFragmenting(disk, fm)
    checksum(disk) shouldBe 2858L
  }
