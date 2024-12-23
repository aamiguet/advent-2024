package ch.aamiguet.advent2024

object Day23 extends Data("data/day23.txt"):

  def parse(lines: List[String]): Map[String, Set[String]] =
    lines.foldLeft(Map.empty[String, Set[String]]): (m, line) =>
      val Array(c1, c2) = line.split("-")
      m
        .updatedWith(c1):
          case Some(s) => Some(s + c2)
          case None => Some(Set(c2))
        .updatedWith(c2):
          case Some(s) => Some(s + c1)
          case None => Some(Set(c1))

  def triplets(compMap: Map[String, Set[String]]): Set[Set[String]] =
    compMap.keySet.foldLeft(Set.empty[Set[String]]): (s, c1) =>
      s ++ compMap(c1)
        .subsets(2)
        .filter: cs =>
          compMap(cs.head)(cs.last) && compMap(cs.head)(cs.last)
        .map(_ + c1)

  def largestSubset(compMap: Map[String, Set[String]]): Set[String] =
    compMap.keySet.foldLeft(Set.empty[String]): (lan, c1) =>
      val r = lan.size to compMap(c1).size
      r.foldLeft(lan): (l, n) =>
        compMap(c1)
          .subsets(n)
          .find(_.subsets(2).forall(pairs => compMap(pairs.head)(pairs.last))) match
          case Some(newLan) => newLan + c1
          case None => l

  def part1(compMap: Map[String, Set[String]]): Int =
    triplets(compMap).filter(s => s.exists(_.startsWith("t"))).size

  def lanPassword(compMap: Map[String, Set[String]]): String =
    largestSubset(compMap).toList.sorted.mkString(",")

  val compMap = parse(lines)

  @main def d23part1(): Unit = println(part1(compMap))
  @main def d23part2(): Unit = println(lanPassword(compMap))
