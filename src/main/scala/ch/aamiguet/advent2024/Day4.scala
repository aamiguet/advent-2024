package ch.aamiguet.advent2024

object Day4 extends App with Data("data/day4.txt"):

  type Coord = (Int, Int)
  type Direction = (Int, Int)

  lazy val directions: List[Direction] = for
    x <- List(-1, 0, 1)
    y <- List(-1, 0, 1) if x != 0 || y != 0
  yield (x, y)

  lazy val grid = gridMap(lines)

  def gridMap(lines: List[String]): Map[Coord, Char] =
    val s = for
      (line, x) <- lines.zipWithIndex
      y <- 0 until line.size
    yield (x, y) -> line(y)
    s.toMap

  def nextCoord(coord: Coord, direction: Direction): Coord =
    (coord._1 + direction._1, coord._2 + direction._2)

  def isWord(coord: Coord, grid: Map[Coord, Char], direction: Direction, word: String): Boolean =
    if word.isEmpty then true
    else
      grid.get(coord) match
        case Some(c) if c == word.head =>
          isWord(nextCoord(coord, direction), grid, direction, word.tail)
        case _ => false

  def wordCount(coord: Coord, grid: Map[Coord, Char]): Int =
    directions.filter(dir => isWord(coord, grid, dir, "XMAS")).size

  def diags(coord: Coord): List[(Coord, Coord)] =
    List(
      ((coord._1 - 1, coord._2 - 1), (coord._1 + 1, coord._2 + 1)),
      ((coord._1 - 1, coord._2 + 1), (coord._1 + 1, coord._2 - 1)),
    )

  def isCross(coord: Coord, grid: Map[Coord, Char]): Boolean =
    if grid(coord) == 'A' then
      diags(coord).forall: (c1, c2) =>
        (grid.get(c1), grid.get(c2)) match
          case (Some(ch1), Some(ch2)) =>
            (ch1 == 'S' && ch2 == 'M') || (ch1 == 'M' && ch2 == 'S')
          case _ => false
    else false

  def wordCount(grid: Map[Coord, Char]): Int =
    grid.keySet.foldLeft(0): (count, coord) =>
      count + wordCount(coord, grid)

  def crossCount(grid: Map[Coord, Char]): Int =
    grid.keySet.foldLeft(0): (count, coord) =>
      if isCross(coord, grid) then count + 1
      else count

  println(wordCount(grid))
  println(crossCount(grid))
