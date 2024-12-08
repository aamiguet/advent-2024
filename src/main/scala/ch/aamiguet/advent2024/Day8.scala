package ch.aamiguet.advent2024

import scala.language.experimental.namedTuples

object Day8 extends Data("data/day8.txt"):

  type Position = (x: Int, y: Int)

  case class City(
      height: Int,
      width: Int,
      antennas: List[(Position, Char)],
  ):
    lazy val locationsByAntennaType: Map[Char, List[Position]] =
      antennas.groupBy(_._2).view.mapValues(_.unzip._1).toMap

    lazy val antennaTypes = locationsByAntennaType.keySet

    def inbound(position: Position): Boolean =
      position.x >= 0 && width > position.x && position.y >= 0 && height > position.y

  object City:
    def apply(lines: List[String]): City =
      val antennas = for
        (line, y) <- lines.zipWithIndex
        (c, x) <- line.zipWithIndex
        if c != '.'
      yield (x, y) -> c
      City(lines.size, lines.head.size, antennas)

  extension (p: Position)
    def unary_- = (-p.x, -p.y)
    def +(other: Position) =
      (p.x + other.x, p.y + other.y)
    def -(other: Position) =
      (p.x - other.x, p.y - other.y)

  def oppositeAntinodes(loc1: Position, loc2: Position)(using city: City): Set[Position] =
    val v: Position = (loc1.x - loc2.x, loc1.y - loc2.y)
    Set(
      loc1 + v,
      loc2 - v,
    ).filter(city.inbound)

  def resonantAntinodes(loc1: Position, loc2: Position)(using city: City): Set[Position] =
    def loop(pos: Position, v: Position, acc: Set[Position] = Set()): Set[Position] =
      if !city.inbound(pos) then acc
      else loop(pos + v, v, acc + pos)
    val v: Position = loc1 - loc2
    loop(loc1, v) ++ loop(loc2, -v)

  def antinodes(locs: List[Position], f: (Position, Position) => Set[Position], acc: Set[Position])(
      using City,
  ): Set[Position] =
    locs
      .combinations(2)
      .foldLeft(acc): (a, ls) =>
        a ++ f(ls.head, ls.last)

  def antinodesPosition(f: (Position, Position) => Set[Position])(using city: City): Set[Position] =
    city.antennaTypes.foldLeft(Set()): (acc, a) =>
      antinodes(city.locationsByAntennaType(a), f, acc)

  def oppositeAntinodesPosition()(using city: City): Set[Position] =
    antinodesPosition(oppositeAntinodes)

  def resonantAntinodePosition()(using city: City): Set[Position] =
    antinodesPosition(resonantAntinodes)

  given city: City = City(lines)

  @main def d8part1(): Unit =
    println(oppositeAntinodesPosition().size)

  @main def d8part2(): Unit =
    println(resonantAntinodePosition().size)
