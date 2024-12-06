package ch.aamiguet.advent2024

import scala.language.experimental.namedTuples

object Day6 extends Data("data/day6.txt"):

  type Coord = (x: Int, y: Int)
  type Direction = (x: Int, y: Int)
  type VisitedTile = (Coord, Direction)

  enum Tile:
    case Free, Start, Block

  val UP = (0, -1)
  val RIGHT = (1, 0)
  val DOWN = (0, 1)
  val LEFT = (-1, 0)

  def nextDir(dir: Direction): Direction = dir match
    case UP => RIGHT
    case RIGHT => DOWN
    case DOWN => LEFT
    case LEFT => UP

  def nextPos(visitedTile: VisitedTile): Coord =
    val (coord, dir) = visitedTile
    (coord.x + dir.x, coord.y + dir.y)

  def parseGrid(lines: List[String]): Map[Coord, Tile] =
    val g = for
      (line, y) <- lines.zipWithIndex
      x <- 0 until line.size
    yield (x, y) -> (line(x) match
      case '#' => Tile.Block
      case '^' => Tile.Start
      case _ => Tile.Free
    )
    g.toMap

  def visitedTiles(currentTile: VisitedTile, visited: Set[VisitedTile] = Set())(using
      grid: Map[Coord, Tile],
  ): Set[VisitedTile] =
    val newVisited = visited + currentTile
    val nextCandPos = nextPos(currentTile)
    grid.get(nextCandPos) match
      case None => newVisited
      case Some(tile) =>
        val (coord, dir) = currentTile
        tile match
          case Tile.Block =>
            val newDir = nextDir(dir)
            visitedTiles((coord, newDir), newVisited)
          case _ =>
            visitedTiles((nextCandPos, dir), newVisited)

  def visitedTileCount()(using grid: Map[Coord, Tile]): Int =
    val startPos = grid.filter((_, tile) => tile == Tile.Start).head._1
    visitedTiles((startPos, UP)).map(_._1).size

  def isLoop(currentTile: VisitedTile, visited: Set[VisitedTile])(using
      grid: Map[Coord, Tile],
  ): Boolean =
    if visited(currentTile) then true
    else
      val newVisited = visited + currentTile
      val (coord, dir) = currentTile
      val nextCandPos = nextPos(currentTile._1, dir)
      grid.get(nextCandPos) match
        case None => false
        case Some(tile) =>
          tile match
            case Tile.Block =>
              val newDir = nextDir(dir)
              isLoop((coord, newDir), newVisited)
            case _ =>
              isLoop((nextCandPos, dir), newVisited)

  def potentialLoopBlockPos(
      currentTile: VisitedTile,
      acc: Set[Coord] = Set(),
      free: Set[Coord] = Set(),
  )(using
      grid: Map[Coord, Tile],
  ): Set[Coord] =
    val (coord, dir) = currentTile
    val nextCandPos = nextPos(currentTile)
    val newFree = free + coord
    grid.get(nextCandPos) match
      case None => acc
      case Some(nextTile) =>
        nextTile match
          case Tile.Block => potentialLoopBlockPos((coord, nextDir(dir)), acc, newFree)
          case Tile.Start => potentialLoopBlockPos((nextPos(coord, dir), dir), acc, newFree)
          case _ =>
            val newAcc =
              lazy val isNew = !acc(nextCandPos)
              // a tile that was visited must stay free!
              lazy val canBlock = !newFree(nextCandPos)
              lazy val isALoop = isLoop((coord, nextDir(dir)), Set(currentTile))(using
                grid.updated(nextCandPos, Tile.Block),
              )
              if isNew && canBlock && isALoop then acc + nextCandPos
              else acc
            potentialLoopBlockPos((nextCandPos, dir), newAcc, newFree)

  def potentialLoopCount()(using grid: Map[Coord, Tile]): Int =
    val startPos = grid.filter((_, tile) => tile == Tile.Start).head._1
    val coords = potentialLoopBlockPos((startPos, UP))
    coords.size

  given grid: Map[Coord, Tile] = parseGrid(lines)

  @main def part1(): Unit = println(visitedTileCount())
  @main def part2(): Unit = println(potentialLoopCount())
