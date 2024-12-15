package ch.aamiguet.advent2024

import scala.language.experimental.namedTuples
import scala.collection.mutable.Map

object Day15 extends Data("data/day15.txt"):

  type Position = (x: Int, y: Int)

  extension (p: Position)
    inline def +(dir: Direction): Position =
      (p.x + dir.pos.x, p.y + dir.pos.y)
    inline def -(dir: Direction): Position =
      (p.x - dir.pos.x, p.y - dir.pos.y)
    inline def expand: Position =
      (p.x * 2, p.y)

  enum Tile:
    case Wall, Box, Empty, Robot, BoxLeft, BoxRight

  object Tile:
    def apply(c: Char): Tile = c match
      case '#' => Wall
      case 'O' => Box
      case '@' => Robot
      case '.' => Empty

  enum Direction(val pos: Position):
    case Up extends Direction((0, -1))
    case Right extends Direction((1, 0))
    case Down extends Direction((0, 1))
    case Left extends Direction((-1, 0))

  object Direction:
    def apply(c: Char): Direction =
      c match
        case '^' => Up
        case '>' => Right
        case 'v' => Down
        case '<' => Left

  def parse(lines: List[String]): (Map[Position, Tile], List[Direction]) =
    val (gridLines, moveLines) = lines.span(_.nonEmpty)
    val grid = for
      (line, y) <- gridLines.zipWithIndex
      (c, x) <- line.zipWithIndex
    yield ((x, y) -> Tile(c))
    val moves = moveLines.mkString.map(Direction.apply)
    (grid.to(Map), moves.toList)

  def printGrid(grid: Map[Position, Tile]): Unit =
    val width = grid.keySet.map(_.x).max
    val height = grid.keySet.map(_.y).max
    for y <- 0 to height
    yield
      val tiles =
        for x <- 0 to width
        yield grid((x, y))
      val line = tiles
        .map:
          case Tile.Wall => '#'
          case Tile.Box => 'O'
          case Tile.BoxLeft => '['
          case Tile.BoxRight => ']'
          case Tile.Empty => '.'
          case Tile.Robot => '@'
        .mkString
      println(line)

  def gpsSum(grid: Map[Position, Tile]): Int =
    grid
      .map:
        case (p, Tile.Box | Tile.BoxLeft) => p.y * 100 + p.x
        case _ => 0
      .sum

  def movedTiles1(
      pos: Position,
      dir: Direction,
      emptyPos: Option[Position] = None,
      firstBoxPos: Option[Position] = None,
  )(using grid: Map[Position, Tile]): (Option[Position], Option[Position]) =
    val currentPos = pos + dir
    grid(currentPos) match
      case Tile.Wall => (None, None)
      case Tile.Box if firstBoxPos.isEmpty =>
        movedTiles1(currentPos, dir, emptyPos, Some(currentPos))
      case Tile.Empty => (Some(currentPos), firstBoxPos)
      case _ => movedTiles1(currentPos, dir, emptyPos, firstBoxPos)

  def part1(lines: List[String]): Int =
    val (grid, moves) = parse(lines)
    var robotPos = grid.find(_._2 == Tile.Robot).head._1
    moves.foreach: m =>
      movedTiles1(robotPos, m)(using grid) match
        case (Some(emptyPos), None) =>
          grid(emptyPos) = Tile.Robot
          grid(robotPos) = Tile.Empty
          robotPos = emptyPos
        case (Some(emptyPos), Some(firstBoxPos)) =>
          grid(emptyPos) = Tile.Box
          grid(firstBoxPos) = Tile.Robot
          grid(robotPos) = Tile.Empty
          robotPos = firstBoxPos
        case _ => ()
    gpsSum(grid)

  def expand(grid: Map[Position, Tile]): Map[Position, Tile] =
    val expanded: Map[Position, Tile] = Map.empty[Position, Tile]
    grid.foreach:
      case (pos, tile) =>
        val p1 = pos.expand
        val p2 = p1 + Direction.Right
        tile match
          case Tile.Wall =>
            expanded(p1) = Tile.Wall
            expanded(p2) = Tile.Wall
          case Tile.Empty =>
            expanded(p1) = Tile.Empty
            expanded(p2) = Tile.Empty
          case Tile.Robot =>
            expanded(p1) = Tile.Robot
            expanded(p2) = Tile.Empty
          case Tile.Box =>
            expanded(p1) = Tile.BoxLeft
            expanded(p2) = Tile.BoxRight
          case _ =>
    expanded

  def moveTilesHorizontally(
      pos: Position,
      dir: Direction,
      acc: Map[Position, Tile] = Map.empty[Position, Tile],
  )(using grid: Map[Position, Tile]): Map[Position, Tile] =
    val currentPos = pos + dir
    grid(currentPos) match
      case Tile.Wall => Map.empty[Position, Tile]
      case Tile.Empty =>
        acc(currentPos) = grid(pos)
        acc
      case _ =>
        acc(currentPos) = grid(pos)
        moveTilesHorizontally(currentPos, dir, acc)

  def moveTilesVertically(
      pos: Set[Position],
      dir: Direction,
      acc: Map[Position, Tile] = Map.empty[Position, Tile],
  )(using grid: Map[Position, Tile]): Map[Position, Tile] =
    if pos.isEmpty then acc
    else
      val currentPos = pos.flatMap: p =>
        val cp = p + dir
        grid(cp) match
          case Tile.BoxLeft => Set(cp, cp + Direction.Right)
          case Tile.BoxRight => Set(cp, cp + Direction.Left)
          case _ => Set(cp)
      if currentPos.exists(cp => grid(cp) == Tile.Wall && grid(cp - dir) != Tile.Empty) then
        Map.empty[Position, Tile]
      else
        currentPos.foreach: cp =>
          val prev = cp - dir
          acc(cp) =
            if pos(prev) then grid(prev)
            else Tile.Empty
        moveTilesVertically(currentPos.filterNot(grid(_) == Tile.Empty), dir, acc)

  def movedTiles2(
      pos: Position,
      dir: Direction,
  )(using grid: Map[Position, Tile]): Map[Position, Tile] = dir match
    case Direction.Right | Direction.Left =>
      moveTilesHorizontally(pos, dir)
    case Direction.Up | Direction.Down =>
      moveTilesVertically(Set(pos), dir)

  // uncomment lines to show a pretty animation
  def part2(lines: List[String]): Int =
    val (g, moves) = parse(lines)
    val grid = expand(g)
    moves.foreach: m =>
      // print("\u001b[2J")
      // println(m)
      val robotPos = grid.find(_._2 == Tile.Robot).head._1
      val mts = movedTiles2(robotPos, m)(using grid)
      if mts.nonEmpty then grid(robotPos) = Tile.Empty
      mts.foreach: m =>
        grid(m._1) = m._2
      // printGrid(grid)
      // println("-" * 50)
      // Thread.sleep(50)
    // printGrid(grid)
    gpsSum(grid)

  @main def d15part1(): Unit = println(part1(lines))
  @main def d15part2(): Unit = println(part2(lines))
