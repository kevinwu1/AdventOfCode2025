import Aoc2025.macros.Macros.logged

import scala.annotation.tailrec
import scala.util.chaining.scalaUtilChainingOps

@scala.annotation.experimental
object Day7 {

  val day = 7
  def main(args: Array[String]): Unit = {
    val test = """.......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............
"""
    val input = os.read(os.pwd / "input" / s"$day.txt")
    println(s"Part 1 test: ${part1(test)}")
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2 test: ${part2(test)}")
    println(s"Part 2: ${part2(input)}")
  }
  def part1(input: String): Int = {
    val lines = input.split("\n")
    val wid = lines.head.length()
    lines.tail
      .foldLeft(
        (lines.head.map(_ == 'S'), 0)
      )({ case ((prevLine, splits), line) =>
        val newsplits = line
          .zip(prevLine)
          .map({
            case ('^', true) =>
              true
            case _ => false
          })
        inline def wasSplit(i: Int): Boolean =
          0 <= i && i < wid && newsplits(i)
        val newLine = (0 until wid).map(i => {
          line(i) != '^' && (prevLine(i) || wasSplit(i - 1) || wasSplit(i + 1))
        })
        (newLine, splits + newsplits.count(identity))
      })
      ._2
  }

  def part2(input: String): Long = {
    val lines = input.split("\n")
    val wid = lines.head.length()
    lines.tail
      .foldLeft(lines.head.map({
        case '.' => 0L
        case 'S' => 1L
      }))({ case (prevLine, line) =>
        val ret = Array.fill(wid)(0L)
        line.zipWithIndex
          .foreach({ case (c, ind) =>
            if (c == '^') {
              ret(ind - 1) += prevLine(ind)
              ret(ind + 1) += prevLine(ind)
            } else {
              ret(ind) += prevLine(ind)
            }
          })
        ret
      })
      .sum
  }
}
