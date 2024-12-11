package ch.aamiguet.advent2024

object Day11 extends Data("data/day11.txt"):

  extension (stone: Long)
    def next: List[Long] =
      val s = stone.toString
      if stone == 0L then List(1L)
      else if s.size % 2 == 0 then
        s.splitAt(s.size / 2) match
          case (s1, s2) => List(s1.toLong, s2.toLong)
      else List(2024 * stone)

  def stoneCount(countMap: Map[Long, Long], blink: Int): Long =
    if blink == 0 then countMap.values.sum
    else
      val newMap = countMap
        .toList
        .flatMap: (stone, count) =>
          stone.next.map(_ -> count)
        .groupBy(_._1)
        .view
        .mapValues(_.map(_._2).sum)
        .toMap
      stoneCount(newMap, blink - 1)

  def stoneCount(input: String, blink: Int): Long =
    val countMap = input.split(" ").map(s => s.toLong -> 1L).toMap
    stoneCount(countMap, blink)

  @main def d11part1(): Unit = println(stoneCount(lines.head, 25))

  @main def d11part2(): Unit = println(stoneCount(lines.head, 75))
