package ch.aamiguet

package object advent2024:
  def timing[A](block: => A): A =
    val start = System.nanoTime()
    val result = block
    val elapsed = (System.nanoTime() - start) / 1000000
    println(s"Elapsed: ${elapsed}ms")
    result
