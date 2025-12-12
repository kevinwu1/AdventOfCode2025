import Aoc2025.macros.Macros.logged

import scala.annotation.tailrec
import scala.util.chaining.scalaUtilChainingOps

@scala.annotation.experimental
object Day12 {

  val day = 12
  def main(args: Array[String]): Unit = {
    val test = """0:
###
##.
##.

1:
###
##.
.##

2:
.##
###
##.

3:
##.
###
##.

4:
###
#..
###

5:
###
.#.
###

4x4: 0 0 0 0 2 0
12x5: 1 0 1 0 2 2
12x5: 1 0 1 0 3 2"""
    val input = os.read(os.pwd / "input" / s"$day.txt")
    println(s"Part 1 test: ${part1(test)}")
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2 test: ${part2(test)}")
    println(s"Part 2: ${part2(input)}")
  }
  def part1(input: String): Long = {
    input
      .split("\n\n")
      .last
      .split("\n")
      .count({ case s"${w}x${h}: $reqs" =>
        reqs.split(" ").map(_.toInt).sum <= (w.toInt / 3) * (h.toInt / 3)
      })
  }

  def part2(input: String): Long = {
    0L
  }
}
