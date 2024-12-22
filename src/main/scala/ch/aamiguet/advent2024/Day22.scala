package ch.aamiguet.advent2024

object Day22 extends Data("data/day22.txt"):

  val MOD = 16777216 // this is 2^24, suspicious!

  def nextSecret(secret: Long): Long =
    val s1 = ((secret * 64) ^ secret) % MOD
    val s2 = ((s1 / 32) ^ s1) % MOD
    val s3 = ((s2 * 2048) ^ s2) % MOD
    s3

  def secretSequence(secret: Long): LazyList[Long] =
    LazyList.iterate(secret)(nextSecret)

  type Sequence = (Long, Long, Long, Long)

  final case class Monkey(init: Long, genSize: Int = 2000):

    lazy val seq: LazyList[Long] =
      secretSequence(init)

    lazy val seqMap: Map[Sequence, Long] =
      val price = seq.map(_ % 10).take(genSize + 1).toList
      val pairs =
        price.tail.zip(price).map((price, prevPrice) => (price, price - prevPrice)) // -1 element !
      val groups = pairs.sliding(4).toList
      groups.foldLeft(Map.empty[Sequence, Long]): (acc, g) =>
        val s = (g(0)._2, g(1)._2, g(2)._2, g(3)._2)
        if !acc.contains(s) then acc.updated(s, g(3)._1)
        else acc

    lazy val lastSecret: Long =
      seq(genSize)

  def mostBananas(monkeys: List[Monkey]): Long =
    val allSeqs = monkeys.flatMap(_.seqMap.keys).toSet
    allSeqs.foldLeft(0L): (currBest, seq) =>
      val cand = monkeys.map(_.seqMap.getOrElse(seq, 0L)).sum
      math.max(currBest, cand)

  def sumOfLastSecret(monkeys: List[Monkey]): Long =
    monkeys.map(_.lastSecret).sum

  def parse(lines: List[String]): List[Monkey] =
    lines.map(l => Monkey(l.toLong))

  lazy val monkeys = parse(lines)

  @main def d22part1(): Unit = println(sumOfLastSecret(monkeys))
  @main def d22part2(): Unit = println(mostBananas(monkeys))
