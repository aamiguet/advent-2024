package ch.aamiguet.advent2024

object Day1 extends App with Data("data/day1.txt"):

  def lists(lines: List[String]): (List[Int], List[Int]) =
    lines.foldLeft((List.empty[Int], List.empty[Int])): (acc, line) =>
      val (left, right) = acc
      val s = line.split("   ")
      (s(0).toInt :: left, s(1).toInt :: right)

  def distance(left: List[Int], right: List[Int]): Int =
    left.sorted.zip(right.sorted).map((l, r) => Math.abs(l - r)).sum

  def similarity(left: List[Int], right: List[Int]): Int =
    left.foldLeft(0): (acc, id) =>
      acc + right.filter(_ == id).size * id

  lazy val (left, right) = lists(lines)

  println(distance(left, right))
  println(similarity(left, right))
