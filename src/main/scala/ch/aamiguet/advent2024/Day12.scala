package ch.aamiguet.advent2024

import scala.language.experimental.namedTuples

object Day12 extends Data("data/day12.txt"):

  type Position = (x: Int, y: Int)
  type Plot = (plant: Char, positions: Set[Position])
  type Fence = (pos: Position, direction: Position)
  type Side = Set[Fence]

  val UP = (0, -1)
  val DOWN = (0, 1)
  val RIGHT = (1, 0)
  val LEFT = (-1, 0)

  val DIRECTIONS: List[Position] = List(UP, RIGHT, DOWN, LEFT)

  extension (p: Position)
    def unary_- = (-p.x, -p.y)
    def +(other: Position) =
      (p.x + other.x, p.y + other.y)
    def -(other: Position) =
      (p.x - other.x, p.y - other.y)

  def parse(lines: List[String]): Map[Position, Char] =
    val m = for
      (line, y) <- lines.zipWithIndex
      (c, x) <- line.zipWithIndex
    yield (x, y) -> c
    m.toMap

  def plot(plant: Char, start: Position)(using farmMap: Map[Position, Char]): Plot =
    def loop(toVisit: Set[Position], acc: Set[Position], visited: Set[Position]): Plot =
      if toVisit.isEmpty then (plant, acc)
      else
        val ps = DIRECTIONS.map(_ + toVisit.head).filter(farmMap.get(_).contains(plant)).toSet
        loop(ps ++ toVisit -- visited, ps ++ acc, visited + toVisit.head)
    loop(Set(start), Set(start), Set.empty[Position])

  def plots(toVisit: Set[Position], acc: List[Plot] = List.empty[Plot])(using
      farmMap: Map[Position, Char],
  ): List[Plot] =
    if toVisit.isEmpty then acc
    else
      val p = plot(farmMap(toVisit.head), toVisit.head)
      plots(toVisit -- p.positions, p :: acc)

  def fences(plot: Plot)(using farmMap: Map[Position, Char]): Set[Fence] =
    def loop(toVisit: List[Position], fenced: Set[Fence]): Set[Fence] =
      if toVisit.isEmpty then
        // println(s"${plot.plant} - ${fenced.size}, price : ${fenced.size * plot.positions.size}")
        fenced
      else
        val toFence: Set[Fence] =
          DIRECTIONS
            .map(d => (d, d + toVisit.head))
            .filterNot:
              case (_, position) => plot.positions(position)
            .map:
              case (direction, _) => (toVisit.head, direction)
            .toSet
        loop(toVisit.tail, fenced ++ toFence)
    loop(plot.positions.toList, Set.empty[Fence])

  def perimeter(plot: Plot)(using farmMap: Map[Position, Char]): Long =
    fences(plot).size.toLong

  def price(plot: Plot)(using farmMap: Map[Position, Char]): Long =
    plot.positions.size * perimeter(plot)

  def totalPrice()(using farmMap: Map[Position, Char]): Long =
    plots(farmMap.keySet).map(price).sum

  def sides(plot: Plot)(using farmMap: Map[Position, Char]): List[Side] =
    val fs = fences(plot)

    def findSide(toVisit: List[Fence], acc: Side): Side =
      if toVisit.isEmpty then acc
      else
        val dirs = toVisit.head.direction match
          case UP | DOWN => List(LEFT, RIGHT)
          case LEFT | RIGHT => List(UP, DOWN)
        val fnext: List[Fence] =
          dirs.map(d => (toVisit.head.pos + d, toVisit.head.direction)).filter(fs).filterNot(acc)
        findSide(fnext ++ toVisit.tail, acc ++ fnext)

    def loop(toVisit: Set[Fence], acc: List[Side]): List[Side] =
      if toVisit.isEmpty then acc
      else
        val side = findSide(List(toVisit.head), Set(toVisit.head))
        loop(toVisit.filterNot(f => side(f)), side :: acc)

    loop(fs, List.empty[Side])

  def discountedPrice(plot: Plot)(using farmMap: Map[Position, Char]): Long =
    plot.positions.size * sides(plot).size

  def discountedTotalPrice()(using farmMap: Map[Position, Char]): Long =
    plots(farmMap.keySet).map(discountedPrice).sum

  given farmMap: Map[Position, Char] = parse(lines)

  @main def d12part1(): Unit = println(totalPrice())
  @main def d12part2(): Unit = println(discountedTotalPrice())
