package ch.aamiguet.advent2024

import org.scalatest.*
import flatspec.*
import matchers.*

import Day2.*

class Day2Spec extends AnyFlatSpec with should.Matchers:

  val test =
    """7 6 4 2 1
      |1 2 7 8 9
      |9 7 6 2 1
      |1 3 2 4 5
      |8 6 4 4 1
      |1 3 6 7 9""".stripMargin

  lazy val reports = parseReports(test.split("\n").toList)

  "Day2" should "identify the number of safe reports" in {
    safeReports(reports).size shouldBe 2
  }

  "Day2" should "identity the number of safe report with the Problem Dampener" in {
    safeReportsDampener(reports).size shouldBe 4
  }
