package ch.aamiguet.advent2024

object Day19 extends Data("data/day19.txt"):

  final case class Onsen(
      towels: List[String],
      designs: List[String],
  ):
    type Arrangement = Vector[String]

    lazy val sortedTowels = towels.sortWith(_.size > _.size)

    lazy val cache: scala.collection.mutable.Map[String, Long] =
      scala.collection.mutable.Map.empty[String, Long]

    def arrangementNumber(design: String): Long =
      cache.getOrElseUpdate(
        design, {
          if design.isEmpty then 1
          else
            val ts = towels.filter(design.startsWith(_))
            ts.foldLeft(0L): (acc, t) =>
              arrangementNumber(design.drop(t.size)) + acc
        },
      )

    private def possibleArrangement(design: String): Option[Arrangement] =
      def loop(toCheck: Vector[(String, Arrangement)]): Option[Arrangement] =
        if toCheck.isEmpty then None
        else
          val tc = toCheck.head
          if tc._1.isEmpty then Some(tc._2)
          else
            val possible = for
              towel <- sortedTowels
              if tc._1.startsWith(towel)
            yield (tc._1.drop(towel.size), tc._2 :+ towel)
            loop(possible.toVector ++ toCheck.tail)
      loop(Vector((design, Vector.empty[String])))

    private def isPossible(design: String): Boolean =
      possibleArrangement(design).nonEmpty

    lazy val possibleDesigns: List[String] =
      designs.filter(isPossible)

    lazy val possibleArrangements: List[Long] =
      designs.map(arrangementNumber)

  object Onsen:
    def apply(lines: List[String]): Onsen =
      Onsen(
        lines.head.split(", ").toList,
        lines.tail.drop(1),
      )

  val onsen = Onsen(lines)

  def part1(onsen: Onsen): Int = onsen.possibleDesigns.size
  def part2(onsen: Onsen): Long = onsen.possibleArrangements.sum

  @main def d19part1(): Unit = println(part1(onsen))
  @main def d19part2(): Unit = println(part2(onsen))
