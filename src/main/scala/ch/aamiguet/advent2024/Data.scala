package ch.aamiguet.advent2024

import scala.io.Source

trait Data(filepath: String):
  lazy val lines = Source.fromFile(filepath).getLines().toList
