package ch.aamiguet.advent2024

import scala.collection.mutable.{Map, ArrayBuffer}
import scala.util.boundary, boundary.break

object Day9 extends Data("data/day9.txt"):

  trait Block:
    def isFree: Boolean
  case object Free extends Block:
    override def isFree: Boolean = true
    override def toString = "."
  final case class File(id: Int) extends Block:
    override def isFree: Boolean = false
    override def toString = id.toString()

  def parse(input: String): (ArrayBuffer[Block], Map[Int, Int]) =
    val a = ArrayBuffer.empty[Block]
    val fm = Map.empty[Int, Int]
    input.zipWithIndex.foldLeft((true, 0)):
      case ((file, id), (c, index)) =>
        val size = c.toString.toInt
        if file then
          a ++= List.fill(size)(File(id))
          (false, id + 1)
        else
          if size > 0 && !fm.isDefinedAt(size) then fm.update(size, a.size)
          a ++= List.fill(size)(Free)
          (true, id)
    (a, fm)

  def compact(disk: ArrayBuffer[Block]): Unit =
    var filePtr = disk.size - 1
    var freePtr = 0
    while
      while !disk(freePtr).isFree do freePtr += 1
      while disk(filePtr).isFree do filePtr -= 1
      freePtr < filePtr
    do
      val free = disk(freePtr)
      val file = disk(filePtr)
      disk(freePtr) = file
      disk(filePtr) = free

  def compactWithoutFragmenting(disk: ArrayBuffer[Block], fm: Map[Int, Int]): Unit =
    def udpateFm(startIndex: Int, endIndex: Int, size: Int): Unit =
      var i = startIndex
      boundary:
        while i < endIndex
        do
          while !disk(i).isFree do i += 1
          var s = 0
          while
            s += 1
            disk(i + s).isFree
          do ()
          if s == size then
            fm(size) = i
            break(())
          i += s + 1

    var filePtr = disk.size - 1
    while !fm.isEmpty
    do
      while disk(filePtr).isFree do filePtr -= 1
      val file = disk(filePtr)
      var fileSize = 0
      while disk(filePtr - fileSize) == file do fileSize += 1
      filePtr -= fileSize
      fm.filter((fs, i) => i > 0 && fileSize <= fs && i < filePtr)
        .toList
        .sortWith(_._2 < _._2)
        .headOption match
        case Some((fs, index)) =>
          for i <- 0 until fileSize
          yield
            disk(i + index) = file
            disk(filePtr + 1 + i) = Free
          fm.remove(fs)
          val rest = fs - fileSize
          if rest > 0 then
            val candIndex = index + fileSize
            if !fm.isDefinedAt(rest) || fm(rest) > candIndex then fm(rest) = candIndex
          udpateFm(index + fileSize, filePtr, fs)
        case _ => ()
      fm.keySet.foreach: k =>
        if fm(k) > filePtr then fm.remove(k)

  def checksum(disk: ArrayBuffer[Block]): Long =
    disk.zipWithIndex.foldLeft(0L):
      case (acc, (File(id), index)) => acc + id * index
      case (acc, _) => acc

  @main def d9part1(): Unit =
    val disk = parse(lines.mkString)._1
    compact(disk)
    println(checksum(disk))

  @main def d9part2(): Unit =
    val (disk, fm) = parse(lines.mkString)
    compactWithoutFragmenting(disk, fm)
    println(checksum(disk))
