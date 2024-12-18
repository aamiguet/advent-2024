package ch.aamiguet.advent2024

import scala.language.experimental.namedTuples

object Day18 extends Data("data/day18.txt"):

  type Position = (x: Int, y: Int)

  enum Direction(val pos: Position):
    case Up extends Direction((0, -1))
    case Right extends Direction((1, 0))
    case Down extends Direction((0, 1))
    case Left extends Direction((-1, 0))

  import Direction.*

  extension (p: Position)
    inline def +(other: Position) =
      (p.x + other.x, p.y + other.y)
    inline def +(dir: Direction) =
      (p.x + dir.pos.x, p.y + dir.pos.y)
    def next =
      List(
        p + Up,
        p + Right,
        p + Down,
        p + Left,
      )

  final case class MemorySpace(
      width: Int,
      height: Int,
      bytes: Map[Position, Int],
  ):
    def inbound(pos: Position): Boolean =
      pos.x >= 0 && pos.x <= width && pos.y >= 0 && pos.y <= height

    def safePos(pos: List[Position], atTime: Int): List[Position] =
      pos.filter(p => inbound(p) && (!bytes.isDefinedAt(p) || bytes(p) > atTime))

  def shortestPath(toVisit: Vector[(Position, Int)], acc: Map[Position, Int], atTime: Int)(using
      memory: MemorySpace,
  ): Map[Position, Int] =
    if toVisit.isEmpty then acc
    else
      val (pos, time) = toVisit.head
      val vs = memory
        .safePos(pos.next, atTime)
        .map((_, time + 1))
        .filter: (p, t) =>
          !acc.isDefinedAt(p) || acc(p) > t
      val newAcc = vs.foldLeft(acc): (a, v) =>
        a.updated(v._1, v._2)
      shortestPath(toVisit.tail ++ vs, newAcc, atTime)

  def parse(lines: List[String], width: Int, height: Int): MemorySpace =
    val bytes = lines
      .map:
        case s"$x,$y" => (x = x.toInt, y = y.toInt)
      .zipWithIndex
      .map:
        case (p, i) => (p, i + 1)
      .toMap
    MemorySpace(width, height, bytes)

  def part1(atTime: Int)(using memory: MemorySpace): Option[Int] =
    val start = ((0, 0), 0)
    val m: Map[Position, Int] = Map((0, 0) -> 0)
    shortestPath(Vector(start), m, atTime: Int).get((memory.width, memory.height))

  def part2(atTime: Int)(using memory: MemorySpace): Position =
    part1(atTime) match
      case Some(_) => part2(atTime + 1)
      case None => memory.bytes.find(_._2 == atTime).head._1

  given memory: MemorySpace = parse(lines, 70, 70)

  @main def d18part1(): Unit = println(part1(1024).head)
  @main def d18part2(): Unit = println(part2(1024))
