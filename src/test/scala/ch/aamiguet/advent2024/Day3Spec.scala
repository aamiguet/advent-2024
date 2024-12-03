package ch.aamiguet.advent2024

import org.scalatest.*
import flatspec.*
import matchers.*

import Day3.*

class Day3Spec extends AnyFlatSpec with should.Matchers:
  val test = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
  val test2 = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

  "Day3" should "find the mul operations and compute the total sum" in {
    multiply(test) shouldBe 161L
  }

  "Day3" should "only multiply enabled operations" in {
    multiplyEnabled(test2) shouldBe 48L
  }
