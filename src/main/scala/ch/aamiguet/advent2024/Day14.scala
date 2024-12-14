package ch.aamiguet.advent2024

import scala.language.experimental.namedTuples

object Day14 extends Data("data/day14.txt"):

  type Position = (x: Int, y: Int)

  extension (p: Position)
    def unary_- = (-p.x, -p.y)
    inline def +(other: Position) =
      (p.x + other.x, p.y + other.y)
    inline def -(other: Position) =
      (p.x - other.x, p.y - other.y)
    inline def *(steps: Int): Position =
      (p.x * steps, p.y * steps)
    inline def %(width: Int, height: Int): Position =
      (math.floorMod(p.x, width), math.floorMod(p.y, height))

  type Robot = (initPos: Position, velocity: Position)

  final case class SecuritySpace(
      robots: List[Robot],
      width: Int,
      height: Int,
  ):

    lazy val initRobotsPos = robots.map(_.initPos).toSet

    def robotPos(time: Int): List[Position] =
      robots.map: robot =>
        (robot.initPos + robot.velocity * time) % (width, height)

    def safetyFactor(time: Int): Int =
      robotPos(time)
        .filter(p => p.x != width / 2 && p.y != height / 2)
        .partition(_.x > width / 2)
        .toList
        .flatMap(_.partition(_.y > height / 2).toList)
        .map(_.size)
        .product

    def stability(time: Int): Int =
      robotPos(time)
        .filter(p => p.x == width / 2 || p.y == height / 2)
        .size

    // draw robot positions and visually identify the first tree we see
    def draw(time: Int): Unit =
      // we only draw robot positions at time with a high stability (# robots in the middle as we expect the tree to be centered 😉)
      if stability(time) >= 25 then
        print("\u001b[2J")
        println(s"Time elpased: $time")
        val pos = robotPos(time).toSet
        (0 until height).map: y =>
          val l = (0 until width).map(x => if pos((x, y)) then '*' else ' ').mkString
          println(l)
        println(s"Time elpased: $time")
        println("-" * width)
        Thread.sleep(10)
      draw(time + 1)

  def parse(lines: List[String], width: Int, height: Int): SecuritySpace =
    val robots = lines.map:
      case s"p=$x,$y v=$vx,$vy" => ((x.toInt, y.toInt), (vx.toInt, vy.toInt))
    SecuritySpace(robots, width, height)

  val securitySpace = parse(lines, 101, 103)

  @main def d14part1(): Unit = println(securitySpace.safetyFactor(100))

  @main def d14part2(): Unit = securitySpace.draw(1)
