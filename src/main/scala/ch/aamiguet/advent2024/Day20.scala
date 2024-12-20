package ch.aamiguet.advent2024

import scala.language.experimental.namedTuples
import scala.collection.mutable.Map as MutMap

object Day20 extends Data("data/day20.txt"):

  type Position = (x: Int, y: Int)
  type ReachablePosition = (pos: Position, time: Int)

  private val ZERO: Position = (0, 0)
  private val UP: Position = (0, -1)
  private val RIGHT: Position = (1, 0)
  private val DOWN: Position = (0, 1)
  private val LEFT: Position = (-1, 0)

  private lazy val reachableCache: MutMap[Int, Set[ReachablePosition]] =
    MutMap.empty[Int, Set[ReachablePosition]]

  private def reachable0(time: Int): Set[ReachablePosition] =
    def loop(
        toVisit: Set[Position],
        t: Int,
        acc: Set[ReachablePosition] = Set.empty[ReachablePosition],
    ): Set[ReachablePosition] =
      if t == time then acc
      else
        val newToVisit: Set[Position] = for
          pos <- toVisit
          n <- pos.next
          if acc.find(_.pos == n).isEmpty
        yield n
        loop(newToVisit, t + 1, acc ++ newToVisit.map((_, t + 1)))
    loop(Set(ZERO), 0)

  extension (p: Position)
    inline def *(steps: Int): Position =
      (p.x * steps, p.y * steps)
    inline def +(other: Position) =
      (p.x + other.x, p.y + other.y)
    def next =
      Set(
        p + UP,
        p + RIGHT,
        p + DOWN,
        p + LEFT,
      )
    def reachable(time: Int): Set[ReachablePosition] =
      reachableCache
        .getOrElseUpdate(time, reachable0(time))
        .map:
          case (pos, time) => (p + pos, time)

  enum Tile:
    case Track, Start, End, Wall
  object Tile:
    def apply(c: Char): Tile = c match
      case '.' => Track
      case '#' => Wall
      case 'E' => End
      case 'S' => Start
  import Tile.*

  extension (t: Tile) def isFree = t != Wall

  final case class Racetrack(
      grid: Map[Position, Tile],
  ):
    private lazy val (startPos, endPos) = (grid.find(_._2 == Start).get._1, grid.find(_._2 == End).get._1)

    private lazy val racetime: Map[Position, Int] =
      def loop(pos: Position, time: Int, acc: Map[Position, Int]): Map[Position, Int] =
        if pos == startPos then acc
        else
          val nextPos = pos.next.filter(p => !acc.isDefinedAt(p) && grid(p).isFree).head
          val newTime = time + 1
          loop(nextPos, newTime, acc.updated(nextPos, newTime))
      loop(endPos, 0, Map(endPos -> 0))

    private lazy val racePos = racetime.keySet

    def possibleCheats(timeGained: Int, timeCheating: Int): Int =
      racePos.foldLeft(0): (acc, pos) =>
        val rt = racetime(pos)
        pos
          .reachable(timeCheating)
          .filter:
            case (rpos, t) => racetime.isDefinedAt(rpos) && rt - (racetime(rpos) + t) >= timeGained
          .size + acc

  object Racetrack:
    def apply(lines: List[String]): Racetrack =
      val grid = for
        (line, y) <- lines.zipWithIndex
        (c, x) <- line.zipWithIndex
      yield ((x, y) -> Tile(c))
      Racetrack(grid.toMap)

  val racetrack = Racetrack(lines)

  @main def d20part1(): Unit = println(racetrack.possibleCheats(100, 2))
  @main def d20part2(): Unit = println(racetrack.possibleCheats(100, 20))
