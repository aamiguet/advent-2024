package ch.aamiguet.advent2024

object Day2 extends App with Data("data/day2.txt"):

  def parseReports(lines: List[String]): List[List[Int]] =
    lines.map(_.split(" ").toList.map(_.toInt))

  def isSafe(head: Int, diff: Int, rest: List[Int]): Boolean =
    if rest.isEmpty then true
    else
      val nextHead = rest.head
      val nextDiff = head - nextHead
      if math.abs(nextDiff) > 3 || nextDiff * diff <= 0 then false
      else isSafe(nextHead, nextDiff, rest.tail)

  def isSafe(levels: List[Int]): Boolean =
    val diff = levels.head - levels.tail.head
    if math.abs(diff) > 3 then false
    else isSafe(levels.tail.head, diff, levels.tail.tail)

  def isSafeDampener(levels: List[Int]): Boolean =
    val size = levels.size
    val range = (0 until size).toList
    val all = levels :: range.map(n => levels.take(n) ++ levels.drop(n + 1))
    all.exists(isSafe)

  lazy val reports = parseReports(lines)

  def safeReports(reports: List[List[Int]]): List[List[Int]] =
    reports.filter(isSafe)

  def safeReportsDampener(reports: List[List[Int]]): List[List[Int]] =
    reports.filter(isSafeDampener)

  println(safeReports(reports).size)
  println(safeReportsDampener(reports).size)
