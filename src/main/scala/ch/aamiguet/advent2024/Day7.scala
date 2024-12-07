package ch.aamiguet.advent2024

import scala.collection.parallel.CollectionConverters.*

object Day7 extends Data("data/day7.txt"):

  case class Equation(
      testValue: Long,
      numbers: List[Long],
  ):

    lazy val isValid: Boolean =
      numbers.tail
        .foldLeft(Set(numbers.head)): (acc, n) =>
          acc.filter(_ <= testValue).flatMap(a => Set(a * n, a + n))
        .contains(testValue)

    lazy val isValidWithConcat: Boolean =
      numbers.tail
        .foldLeft(Set(numbers.head)): (acc, n) =>
          acc.filter(_ <= testValue).flatMap(a => Set(a * n, a + n, s"$a$n".toLong))
        .contains(testValue)

  object Equation:
    def apply(line: String): Equation = line match
      case s"$t: $ns" => Equation(t.toLong, ns.split(" ").toList.map(_.toLong))

  def parseEquations(lines: List[String]): List[Equation] =
    lines.map(Equation.apply)

  val equations = parseEquations(lines)

  def totalCalibrationResult(equations: List[Equation]): Long =
    equations.par.filter(_.isValid).map(_.testValue).sum

  def totalCalibrationResultWithConcat(equations: List[Equation]): Long =
    equations.par.filter(_.isValidWithConcat).map(_.testValue).sum

  @main def d7part1(): Unit = println(totalCalibrationResult(equations))
  @main def d7part2(): Unit = println(totalCalibrationResultWithConcat(equations))
