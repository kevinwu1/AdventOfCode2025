import Aoc2025.macros.Macros.logged

import scala.annotation.tailrec
import scala.util.chaining.scalaUtilChainingOps

@scala.annotation.experimental
object Day6 {

  val day = 6
  def main(args: Array[String]): Unit = {
    val test =
      """123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  """
    val input = os.read(os.pwd / "input" / s"$day.txt")
    println(s"Part 1 test: ${part1(test)}")
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2 test: ${part2(test)}")
    println(s"Part 2: ${part2(input)}")
  }
  def part1(input: String): Long = {
    val spl = input
      .split("\n")
    val nums = spl
      .slice(0, spl.length - 1)
      .map(_.split("\\s+").filter(_.nonEmpty).map(_.toLong))
    val ops = spl.last.split("\\s+")
    (0 until ops.size)
      .map(ind =>
        (0 until nums.size)
          .map(nr => nums(nr)(ind))
          .reduce(ops(ind) match {
            case "*" => _ * _
            case "+" => _ + _
          })
      )
      .sum
  }

  def part2(input: String): Long = {
    val spl = input
      .split("\n")
    val nums = spl.slice(0, spl.length - 1).map(x => x + " 0")
    val ops = (spl.last + " +").zipWithIndex.filter(_._1 != ' ')
    val rows = nums.size
    val cols = nums.head.size
    val tra =
      (0 until cols)
        .map(c => {
          (0 until rows)
            .map(r => {
              nums(r)(c)
            })
            .mkString
        })
    println(tra)

    println(nums.transpose[Char](using x => x.toCharArray()))
    ops
      .sliding(2)
      .map({ case Seq((op, ind), (_, nextInd)) =>
        (0 until (nextInd - ind - 1))
          .map(off => tra(ind + off).replace(" ", "").toLong)
          .reduce(op match {
            case '*' => _ * _
            case '+' => _ + _
          })
      })
      .sum
  }
}
