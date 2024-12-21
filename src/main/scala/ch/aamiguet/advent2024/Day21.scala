package ch.aamiguet.advent2024

import scala.language.experimental.namedTuples
import scala.collection.mutable.Map as MutMap

object Day21 extends Data("data/day21.txt"):

  type Position = (x: Int, y: Int)
  enum Direction(val pos: Position, val c: Char):
    case Up extends Direction((0, -1), '^')
    case Right extends Direction((1, 0), '>')
    case Down extends Direction((0, 1), 'v')
    case Left extends Direction((-1, 0), '<')
    override def toString(): String = c.toString

  import Direction.*

  extension (p: Position)
    inline def +(other: Position) =
      (p.x + other.x, p.y + other.y)
    inline def *(steps: Int): Position =
      (p.x * steps, p.y * steps)
    inline def +(dir: Direction) =
      (p.x + dir.pos.x, p.y + dir.pos.y)

    def dist(other: Position) =
      math.abs(p.x - other.x) + math.abs(p.y - other.y)
    def next =
      List(
        (p + Up, Up),
        (p + Right, Right),
        (p + Down, Down),
        (p + Left, Left),
      )

  trait Pad:
    type Path = Vector[Direction]

    val pad: Map[Char, Position]
    lazy val padPos = pad.values.toSet
    lazy val pathCache: MutMap[(Char, Char), Path] = MutMap.empty[(Char, Char), Path]

    given ppOrd: Ordering[(Position, Path)] with
      def compare(t1: (Position, Path), t2: (Position, Path)) =
        t1._2.distinct.size.compare(t2._2.distinct.size)

    def shortestPath(from: Position, to: Position): Path =
      val (vertSteps, vertDir) =
        val diff = from.y - to.y
        if diff < 0 then (math.abs(diff), Down) else (diff, Up)
      val (horSteps, horDir) =
        val diff = from.x - to.x
        if diff < 0 then (math.abs(diff), Right) else (diff, Left)
      if horDir == Left && padPos(from + (horDir.pos * horSteps)) then
        Vector.fill(horSteps)(horDir) ++ Vector.fill(vertSteps)(vertDir)
      else if vertDir == Down && padPos(from + (vertDir.pos * vertSteps)) then
        Vector.fill(vertSteps)(vertDir) ++ Vector.fill(horSteps)(horDir)
      else if padPos(from + (horDir.pos * horSteps)) then
        Vector.fill(horSteps)(horDir) ++ Vector.fill(vertSteps)(vertDir)
      else Vector.fill(vertSteps)(vertDir) ++ Vector.fill(horSteps)(horDir)

    def path(from: Char, to: Char): Path =
      pathCache.getOrElseUpdate((from, to), shortestPath(pad(from), pad(to)))

  object NumPad extends Pad:
    val pad: Map[Char, Position] = Map(
      '7' -> (0, 0),
      '8' -> (1, 0),
      '9' -> (2, 0),
      '4' -> (0, 1),
      '5' -> (1, 1),
      '6' -> (2, 1),
      '1' -> (0, 2),
      '2' -> (1, 2),
      '3' -> (2, 2),
      '0' -> (1, 3),
      'A' -> (2, 3),
    )

  object DirectionalPad extends Pad:
    val pad: Map[Char, Position] = Map(
      '^' -> (1, 0),
      'A' -> (2, 0),
      '<' -> (0, 1),
      'v' -> (1, 1),
      '>' -> (2, 1),
    )

  def shortestSequence(line: String, pads: List[Pad]): String =
    val seq = pads
      .foldLeft(line):
        case (l, p) =>
          l.foldLeft(("", 'A')):
            case ((acc, from), to) =>
              (acc + p.path(from, to).mkString + "A", to)
          ._1
    seq

  def numPart(line: String): Int =
    line.dropRight(1).toInt

  def part1(lines: List[String]): Int =
    val pads = List(NumPad, DirectionalPad, DirectionalPad)
    lines.map(l => numPart(l) * shortestSequence(l, pads).size).sum

  def part2(lines: List[String]): Int =
    val pads = List(NumPad) ++ List.fill(11)(DirectionalPad)
    lines.map(l => numPart(l) * shortestSequence(l, pads).size).sum

  @main def d21part1(): Unit =
    println(part1(lines))

  @main def d21part2(): Unit =
    println(part2(lines))
