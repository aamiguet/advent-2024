package ch.aamiguet.advent2024

object Day3 extends App with Data("data/day3.txt"):

  def multiply(memory: String): Long =
    val regex = """mul\((\d+),(\d+)\)""".r
    val matches = regex.findAllMatchIn(memory).toList
    matches
      .map: m =>
        val x = m.group(1).toLong
        val y = m.group(2).toLong
        x * y
      .sum

  def enabledMemory(memory: String): String =
    memory.indexOf("do()") match
      case n if n > 0 => memory.drop(n + 4)
      case _ => ""

  def multiplyEnabled(memory: String, acc: Long = 0L): Long =
    if memory.isEmpty then acc
    else
      memory.indexOf("don't()") match
        case n if n > 0 =>
          val curr = multiply(memory.take(n))
          multiplyEnabled(enabledMemory(memory.drop(n + 7)), acc + curr)
        case _ => multiply(memory) + acc

  println(multiply(lines.mkString))
  println(multiplyEnabled(lines.mkString))
