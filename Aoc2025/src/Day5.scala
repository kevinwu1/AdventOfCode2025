import Aoc2025.macros.Macros.logged

import scala.annotation.tailrec
import scala.util.chaining.scalaUtilChainingOps

@scala.annotation.experimental
object Day5 {

  val day = 5
  def main(args: Array[String]): Unit = {
    val test = """3-5
10-14
16-20
12-18

1
5
8
11
17
32
"""
    val input = os.read(os.pwd / "input" / s"$day.txt")
    println(s"Part 1 test: ${part1(test)}")
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2 test: ${part2(test)}")
    val t = System.nanoTime()
    println(s"Part 2: ${part2(input)}")
    println((System.nanoTime() - t) / 1e9)
  }
  def part1(input: String): Int = {
    val Array(first, second) = input.split("\n\n")
    val ranges = first
      .split("\n")
      .map({ case s"$s-$e" => (s.toLong, e.toLong) })
    val queries = second.split("\n").map(_.toLong)
    queries.count(c => ranges.exists(r => r._1 <= c && c <= r._2))
  }

  def part2(input: String): Long = {
    input
      .split("\n\n")
      .head
      .split("\n")
      .flatMap({ case s"$s-$e" => Vector((s.toLong, 1), (e.toLong + 1, -1)) })
      .sortBy(_._1)
      .foldLeft(
        (0L, -1L, 0)
      )({ case ((total, start, count), (nextInd, nextCount)) =>
        val newCount = count + nextCount
        (
          if (newCount == 0) total + nextInd - start else total,
          if (count == 0) nextInd else start,
          newCount
        )
      })
      ._1
  }
}
