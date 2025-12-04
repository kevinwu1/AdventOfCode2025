import scala.util.chaining.scalaUtilChainingOps

object Day3 {

  val day = 3
  def main(args: Array[String]): Unit = {
    val test = """987654321111111
811111111111119
234234234234278
818181911112111"""
    val input = os.read(os.pwd / "input" / s"$day.txt")
    println(s"Part 1 test: ${part1(test)}")
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2 test: ${part2(test)}")
    println(s"Part 2: ${part2(input)}")
  }

  def part1(input: String): Int = {
    input
      .split("\n")
      .map(line => {
        val digits = line.split("").map(_.toInt)
        val first = digits.slice(0, digits.length - 1)
        val maxDig1 = first.max
        val firstPos = first.indexWhere(_ == maxDig1)
        val maxDig2 = digits.slice(firstPos + 1, digits.length).max
        maxDig1 * 10 + maxDig2
      })
      .sum
  }
  def part2(input: String): Long = {
    input
      .split("\n")
      .map(line => {
        val digits = line.split("").map(_.toLong)
        def getBest(accum: Long = 0, remaining: Int = 12, startFrom: Int = 0)
            : Long = {
          if (remaining == 0)
            accum
          else {
            val window =
              digits.slice(startFrom, digits.length - (remaining - 1))
            val maxD = window.max
            val maxPos = window.indexWhere(_ == maxD)
            getBest(accum * 10 + maxD, remaining - 1, startFrom + 1 + maxPos)
          }

        }
        getBest()
      })
      .sum
  }
}
