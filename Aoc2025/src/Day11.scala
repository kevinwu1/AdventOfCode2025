import Aoc2025.macros.Macros.logged

@scala.annotation.experimental
object Day11 {

  val day = 11
  def main(args: Array[String]): Unit = {
    val test = """aaa: you hhh
you: bbb ccc
bbb: ddd eee
ccc: ddd eee fff
ddd: ggg
eee: out
fff: out
ggg: out
hhh: ccc fff iii
iii: out"""

    val test2 = """svr: aaa bbb
aaa: fft
fft: ccc
bbb: tty
tty: ccc
ccc: ddd eee
ddd: hub
hub: fff
eee: dac
dac: fff
fff: ggg hhh
ggg: out
hhh: out"""
    val input = os.read(os.pwd / "input" / s"$day.txt")
    println(s"Part 1 test: ${part1(test)}")
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2 test: ${part2(test2)}")
    println(s"Part 2: ${part2(input)}")
  }
  case class GraphContext(
      nodes: Set[String],
      inputs: Map[String, Vector[String]]
  )
  extension (s: String) {
    def pathsTo(other: String)(using gc: GraphContext): Long =
      getPaths(
        gc.nodes,
        gc.inputs,
        s,
        other
      )
  }

  def part1(input: String): Long = {
    val outputs = input
      .split("\n")
      .map({ case s"$from: $to" =>
        from -> to.split(" ").toVector
      })
      .toMap
      .withDefaultValue(Vector())
    val inputs: Map[String, Vector[String]] = outputs.toVector
      .flatMap((a, b) => b.map(bi => a -> bi))
      .groupBy(_._2)
      .mapValues(_.map(_._1).toVector)
      .toMap
      .withDefaultValue(Vector())
    val nodes = outputs.keySet ++ inputs.keySet
    given GraphContext(nodes, inputs)
    "you".pathsTo("out")
  }

  def part2(input: String): Long = {
    val outputs = input
      .split("\n")
      .map({ case s"$from: $to" =>
        from -> to.split(" ").toVector
      })
      .toMap
      .withDefaultValue(Vector())
    val inputs: Map[String, Vector[String]] = outputs.toVector
      .flatMap((a, b) => b.map(bi => a -> bi))
      .groupBy(_._2)
      .mapValues(_.map(_._1).toVector)
      .toMap
      .withDefaultValue(Vector())
    val nodes = outputs.keySet ++ inputs.keySet
    given GraphContext(nodes, inputs)
    Vector(
      Vector("svr", "dac", "fft", "out"),
      Vector("svr", "fft", "dac", "out")
    ).map(v => v.sliding(2).map { case Vector(a, b) => a.pathsTo(b) }.product)
      .sum
  }

  def getPaths(
      nodes: Set[String],
      inputs: Map[String, Vector[String]],
      start: String,
      end: String
  ): Long = {
    import scala.collection.mutable
    val ans = mutable.Map[String, Int]()
    ans(start) = 1
    def explore(): Unit = {
      (nodes -- ans.keySet)
        .filter(n => inputs(n).forall(ans.contains))
        .foreach(n => {
          ans(n) = inputs(n).map(ans).sum
        })
    }
    while (!ans.contains(end))
      explore()
    ans(end)
  }

}
