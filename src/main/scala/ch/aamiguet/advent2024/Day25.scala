package ch.aamiguet.advent2024

object Day25 extends Data("data/day25.txt"):

  def lockOrKey(lines: List[String]): Either[Array[Int], Array[Int]] =
    val arr = Array.fill[Int](5)(-1)
    lines.foreach: l =>
      l.zipWithIndex.foreach: (c, i) =>
        if c == '#' then arr(i) += 1
    if lines.head.forall(_ == '#') then Left(arr) else Right(arr)

  def isAFit(lock: Array[Int], key: Array[Int]): Boolean =
    lock.zip(key).forall(_ + _ <= 5)

  def parse(lines: List[String]): (List[Array[Int]], List[Array[Int]]) =
    lines.grouped(8).toList.partitionMap(lockOrKey)

  def fitCount(locks: List[Array[Int]], keys: List[Array[Int]]): Int =
    val fitPairs = for
      l <- locks
      k <- keys
      if isAFit(l, k)
    yield (k, l)
    fitPairs.size

  val (locks, keys) = parse(lines)

  @main def d25part1(): Unit = println(fitCount(locks, keys))
