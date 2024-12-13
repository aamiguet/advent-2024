package ch.aamiguet.advent2024

object Day13 extends Data("data/day13.txt"):

  type Matrix = Array[Array[Long]]

  final case class ClawMachine(
      matrix: Matrix,
  ):
    lazy val det = matrix(0)(0) * matrix(1)(1) - matrix(0)(1) * matrix(1)(0)
    lazy val a = 1d * (matrix(2)(0) * matrix(1)(1) - matrix(2)(1) * matrix(1)(0)) / det
    lazy val b = 1d * (matrix(0)(0) * matrix(2)(1) - matrix(0)(1) * matrix(2)(0)) / det

    lazy val fewestTokens =
      if a >= 0 && b >= 0 && a.isWhole && b.isWhole then a.toLong * 3 + b.toLong
      else 0

  object ClawMachine:
    def apply(lines: List[String]): ClawMachine =
      val matrix: Matrix = Array.fill(3)(Array.fill(2)(0))
      lines(0) match
        case s"Button A: X+$x1, Y+$x2" =>
          matrix(0)(0) = x1.toLong
          matrix(0)(1) = x2.toLong
      lines(1) match
        case s"Button B: X+$y1, Y+$y2" =>
          matrix(1)(0) = y1.toLong
          matrix(1)(1) = y2.toLong
      lines(2) match
        case s"Prize: X=$p1, Y=$p2" =>
          matrix(2)(0) = p1.toLong
          matrix(2)(1) = p2.toLong
      ClawMachine(matrix)

  def parse(lines: List[String]): List[ClawMachine] =
    lines.grouped(4).map(ClawMachine.apply).toList

  def fewestTokens(cms: List[ClawMachine]): Long =
    cms.map(_.fewestTokens).sum

  def corrected(cms: List[ClawMachine]): List[ClawMachine] =
    cms.map: cm =>
      val nm = cm.matrix.updated(2, cm.matrix(2).map(_ + 10000000000000L))
      ClawMachine(nm)

  val cms = parse(lines)

  @main def d13part1(): Unit =
    println(fewestTokens(cms))

  @main def d13part2(): Unit =
    println(fewestTokens(corrected(cms)))
