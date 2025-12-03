//> using dep com.lihaoyi::os-lib:0.11.6

import scala.util.chaining.scalaUtilChainingOps

object Day1 {

  val day = 1
  def main(args: Array[String]): Unit = {
    val test = """L68
L30
R48
L5
R60
L55
L1
L99
R14
L82"""
    val input = os.read(os.pwd / "input" / s"$day.txt")
    println(s"Part 1 test: ${part1(test)}")
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2 test: ${part2(test)}")
    println(s"Part 2: ${part2(input)}")
  }

  def part1(input: String): String = {
    input
      .split("\n")
      .map({
        case s"L$d" => -d.toInt
        case s"R$d" => d.toInt
      })
      .foldLeft((50, 0))({ case ((position, points), add) =>
        val next = (position + add) %% 100
        (next, points + (if (next == 0) 1 else 0))
      })
      ._2
      .toString()
  }
  def part2(input: String): String = {
    input
      .split("\n")
      .map({
        case s"L$d" => -d.toInt
        case s"R$d" => d.toInt
      })
      .foldLeft((50, 0))({ case ((prev, total), add) =>
        val next = prev + add
        val points =
          next.abs / 100 + (
            // +1 for getting to 0 only if i wasn't already at 0
            if (prev != 0 && next <= 0) 1 else 0
          )
        (next %% 100, total + points)
      })
      ._2
      .toString()
  }
  extension (i: Int) def %%(m: Int): Int = {
    val r = i % m
    if (r < 0)
      r + m
    else r
  }
}
