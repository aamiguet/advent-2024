package ch.aamiguet.advent2024

import scala.language.experimental.namedTuples

object Day10 extends Data("data/day10.txt"):

  type Position = (x: Int, y: Int)
  type Path = List[Position]

  val DIRECTIONS: List[Position] = List(
    (1, 0),
    (-1, 0),
    (0, 1),
    (0, -1),
  )

  extension (p: Position)
    def unary_- = (-p.x, -p.y)
    def +(other: Position) =
      (p.x + other.x, p.y + other.y)
    def -(other: Position) =
      (p.x - other.x, p.y - other.y)

  def parse(lines: List[String]): Map[Position, Int] =
    val l = for
      (line, y) <- lines.zipWithIndex
      (c, x) <- line.zipWithIndex
    yield (x, y) -> c.toString.toInt
    l.toMap

  def startPos()(using map: Map[Position, Int]): List[Position] =
    map.filter(_._2 == 0).keys.toList

  def score(startPos: Position)(scoreFn: List[Path] => Int)(using map: Map[Position, Int]): Int =
    def next(path: Path): List[Path] =
      val pos = path.head
      val height = map(pos)
      val nextPos = DIRECTIONS
        .map(_ + pos)
        .filter: p =>
          map.get(p) match
            case Some(h) if h == height + 1 => true
            case _ => false
      nextPos.map(_ :: path)

    def loop(paths: List[Path]): Int =
      if paths.isEmpty then 0
      else if map(paths.head.head) == 9 then scoreFn(paths)
      else
        val nextPaths = paths.flatMap(next)
        loop(nextPaths)
    loop(List(List(startPos)))

  def totalScore()(using map: Map[Position, Int]): Int =
    startPos().map(score(_)(ps => ps.map(_.head).distinct.size)).sum

  def totalScoreAlt()(using map: Map[Position, Int]): Int =
    startPos().map(score(_)(ps => ps.size)).sum

  given map: Map[Position, Int] = parse(lines)

  @main def d10part1(): Unit = println(totalScore())
  @main def d10part2(): Unit = println(totalScoreAlt())
