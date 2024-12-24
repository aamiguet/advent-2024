package ch.aamiguet.advent2024

object Day24 extends Data("data/day24.txt"):

  extension (b: Boolean) def toBit = if b then 1 else 0

  trait Logic extends Function2[Boolean, Boolean, Boolean]
  case object Xor extends Logic:
    override def apply(v1: Boolean, v2: Boolean): Boolean = v1 ^ v2
    override def toString() = "XOR"
  case object And extends Logic:
    override def apply(v1: Boolean, v2: Boolean): Boolean = v1 && v2
    override def toString() = "AND"
  case object Or extends Logic:
    override def apply(v1: Boolean, v2: Boolean): Boolean = v1 || v2
    override def toString() = "OR"

  object Logic:
    def apply(l: String): Logic = l match
      case "XOR" => Xor
      case "AND" => And
      case "OR" => Or

  final case class Gate(a: String, b: String, out: String, logic: Logic):
    def fitIn(x: String, y: String, l: Logic): Boolean =
      l == logic && ((x == a && y == b) || (x == b && y == a))
    def fitOut(x: String, z: String, l: Logic): Boolean =
      l == logic && ((x == a || x == b) && z == out)

  def logic(l: String): (Boolean, Boolean) => Boolean = l match
    case "XOR" => _ ^ _
    case "AND" => _ && _
    case "OR" => _ || _

  def parse(lines: List[String]): (Map[String, Boolean], List[Gate]) =
    val (s, g) = lines.span(_.nonEmpty)
    val state = s
      .map:
        case s"$a: $v" => a -> (v == "1")
      .toMap
    val gates = g.tail.map:
      case s"$a $l $b -> $out" => Gate(a, b, out, Logic(l))
    (state, gates)

  def finalState(state: Map[String, Boolean], gates: List[Gate]): Map[String, Boolean] =
    if gates.isEmpty then state
    else
      val (ns, ng) = gates.partitionMap: g =>
        (state.get(g.a), state.get(g.b)) match
          case (Some(a), Some(b)) => Left(g.out -> g.logic(a, b))
          case _ => Right(g)
      finalState(state ++ ns, ng)

  def systemValue(state: Map[String, Boolean]): Long =
    state.toList
      .filter(_._1.startsWith("z"))
      .sorted
      .map(_._2.toBit)
      .zipWithIndex
      .map(_ * math.pow(2, _).toLong)
      .sum

  def part1(state: Map[String, Boolean], gates: List[Gate]): Long =
    val fs = finalState(state, gates)
    systemValue(fs)

  extension (s: String) def id(index: Int) = s"$s${"%02d".format(index)}"

  def findSwaps(gates: Set[Gate], index: Int, carry: String): Unit =
    if index > 44 then println("no swaps!")
    else
      val x = "x".id(index)
      val y = "y".id(index)
      val z = "z".id(index)
      val xyXor = gates.find(_.fitIn(x, y, Xor)).head
      val xyAnd = gates.find(_.fitIn(x, y, And)).head
      val andCarry = gates.find(_.fitIn(xyXor.out, carry, And)).head
      val _ = gates.find(_.fitOut(carry, z, Xor)).head
      val carryOutput = gates.find(_.fitIn(andCarry.out, xyAnd.out, Or)).head
      findSwaps(gates, index + 1, carryOutput.out)

  lazy val (state, gates) = parse(lines)

  @main def d24part1() = println(part1(state, gates))
  @main def d24part2() =
    val swaps = List(
      "fph" -> "z15",
      "z21" -> "gds",
      "wrk" -> "jrs",
      "z34" -> "cqk",
    )
    val fixedGates = swaps.foldLeft(gates.toSet): (gs, s) =>
      gs.map: g =>
        if g.out == s._1 then g.copy(out = s._2)
        else if g.out == s._2 then g.copy(out = s._1)
        else g
    findSwaps(fixedGates, 1, fixedGates.find(_.fitIn("x00", "y00", And)).head.out)
    println(swaps.flatMap(s => List(s._1, s._2)).sorted.mkString(","))
