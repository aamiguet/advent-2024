package ch.aamiguet.advent2024

object Day5 extends App with Data("data/day5.txt"):

  type Update = List[Int]

  case class PrintQueue(
      afterPages: Map[Int, Set[Int]],
      updates: List[Update],
  ):

    private def isValid(page: Int, rest: Update): Boolean =
      afterPages.get(page) match
        case Some(pages) if rest.exists(pages(_)) => false
        case _ => true

    private def isValid(update: Update): Boolean =
      if update.isEmpty then true
      else if isValid(update.head, update.tail) then isValid(update.tail)
      else false

    private def validPart(update: Update, valid: Update = Nil): Update =
      if update.isEmpty then valid.reverse
      else if isValid(update.head, update.tail) then validPart(update.tail, update.head :: valid)
      else valid.reverse

    def fixInvalid(rest: Update, acc: Update = Nil): Update =
      val valid = validPart(rest)
      val newAcc = acc ++ valid
      if valid.size == rest.size then newAcc
      else
        val invalid = rest.drop(valid.size)
        val pages = afterPages(invalid.head)
        val swapPos = invalid.indexWhere(pages.contains(_))
        val newRest = invalid.updated(0, invalid(swapPos)).updated(swapPos, invalid.head)
        fixInvalid(newRest, newAcc)

    lazy val fixedInvalidUpdates: List[Update] =
      invalidUpdates.map(fixInvalid(_))

    lazy val (validUpdates, invalidUpdates) =
      updates.partition(isValid)

  object PrintQueue:
    def apply(lines: List[String]): PrintQueue =
      val (rs, us) = lines.span(_.nonEmpty)
      val afterPages = rs
        .map:
          case s"$p1|$p2" => p2.toInt -> p1.toInt
        .groupBy(_._1)
        .mapValues(_.map(_._2).toSet)
        .toMap
      val updates = us.tail.map:
        _.split(",").toList.map(_.toInt)
      PrintQueue(afterPages, updates)

  def middlePage(update: Update): Int =
    update(update.size / 2)

  def sumOfMiddlePage(updates: List[Update]): Int =
    updates.map(middlePage).sum

  lazy val printQueue = PrintQueue(lines)

  println(sumOfMiddlePage(printQueue.validUpdates))
  println(sumOfMiddlePage(printQueue.fixedInvalidUpdates))
