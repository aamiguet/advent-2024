package ch.aamiguet.advent2024

import scala.language.experimental.namedTuples

object Day16 extends Data("data/day16.txt"):

  type Position = (x: Int, y: Int)

  extension (p: Position)
    inline def +(dir: Direction): Position =
      (p.x + dir.v.x, p.y + dir.v.y)

  enum Direction(val v: Position):
    case Up extends Direction((0, -1))
    case Right extends Direction((1, 0))
    case Down extends Direction((0, 1))
    case Left extends Direction((-1, 0))
  import Direction.*

  extension (d: Direction)
    def rotateClockwise: Direction = d match
      case Up => Right
      case Right => Down
      case Down => Left
      case Left => Up

    def rotateAnticlockwise: Direction = d match
      case Up => Left
      case Left => Down
      case Down => Right
      case Right => Up

  enum Tile:
    case Start, End, Empty, Wall
  object Tile:
    def apply(c: Char): Tile = c match
      case '#' => Wall
      case '.' => Empty
      case 'S' => Start
      case 'E' => End
  import Tile.*

  def parse(lines: List[String]): Map[Position, Tile] =
    val m = for
      (line, y) <- lines.zipWithIndex
      (c, x) <- line.zipWithIndex
    yield (x, y) -> Tile(c)
    m.toMap

  type Path = List[Position]
  type Visit = (pos: Position, dir: Direction, cost: Int, path: Path)

  extension (v: Visit)
    def next()(using maze: Map[Position, Tile]): List[Visit] =
      def next(dir: Direction, addCost: Int): Visit =
        val newPos = v.pos + dir
        (newPos, dir, v.cost + addCost, newPos :: v.path)
      val forward: Visit = next(v.dir, 1)
      val left: Visit = next(v.dir.rotateAnticlockwise, 1001)
      val right: Visit = next(v.dir.rotateClockwise, 1001)
      val backward: Visit = next(v.dir.rotateClockwise.rotateClockwise, 2001)
      List(forward, left, right, backward).filter(v => maze.getOrElse(v.pos, Wall) != Wall)

  def costMap(
      toVisit: List[Visit],
      acc: Map[Position, Map[Direction, (Int, List[Path])]] =
        Map.empty[Position, Map[Direction, (Int, List[Path])]],
  )(using maze: Map[Position, Tile]): Map[Position, Map[Direction, (Int, List[Path])]] =
    if toVisit.isEmpty then acc
    else
      val v: Visit = toVisit.head
      val next = v.next()
      val newAcc = acc.updatedWith(v.pos):
        case Some(m) =>
          Some {
            m.updatedWith(v.dir):
              case Some((c, path)) =>
                if c == v.cost then Some((c, v.path :: path))
                else Some((v.cost, List(v.path)))
              case None => Some((v.cost, List(v.path)))
          }
        case None => Some(Map(v.dir -> (v.cost, List(v.path))))
      val newToVisit = (toVisit.tail ++ next).filter: v =>
        newAcc.get(v.pos).flatMap(_.get(v.dir)) match
          case Some((c, _)) => c >= v.cost
          case None => true
      costMap(newToVisit, newAcc)

  def lowestScore()(using maze: Map[Position, Tile]): Int =
    val startPos = maze.find(_._2 == Start).head._1
    val endPos = maze.find(_._2 == End).head._1
    val cm = costMap(List((startPos, Right, 0, List(startPos))))
    cm(endPos).toList.sortWith(_._2._1 < _._2._1).head._2._1

  def bestTiles()(using maze: Map[Position, Tile]): Int =
    val startPos = maze.find(_._2 == Start).head._1
    val endPos = maze.find(_._2 == End).head._1
    val cm = costMap(List((startPos, Right, 0, List(startPos))))
    cm(endPos).toList.sortWith(_._2._1 < _._2._1).head._2._2.reduce(_ ++ _).toSet.size

  given maze: Map[Position, Tile] = parse(lines)

  @main def d16part1(): Unit = println(lowestScore())
  @main def d16part2(): Unit = println(bestTiles())
