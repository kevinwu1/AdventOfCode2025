import scala.util.chaining.scalaUtilChainingOps

object Day2 {

  val day = 2
  def main(args: Array[String]): Unit = {
    val test =
      """11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"""
    val input = os.read(os.pwd / "input" / s"$day.txt")
    "asdf".tail
    println(s"Part 1 test: ${part1(test)}")
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2 test: ${part2(test)}")
    println(s"Part 2: ${part2(input)}")
    // (2 to 64)
    //   .map(x => x -> divisors(x))
    //   .mkString("\n")
    //   .pipe(println(_))
  }

  def part1(input: String): Long = {
    input
      .split(",")
      .map({ case s"$lo-$hi" => (lo.toLong, hi.toLong) })
      .map({ case (a, b) =>
        (a to b)
          .filter(i => {
            val is = i.toString()
            is.length() % 2 == 0 && {
              val mid = is.length() / 2
              is.slice(0, mid) == is.slice(mid, is.length())
            }
          })
          .foldLeft(0L)(_ + _)
      })
      .sum
  }
  def part2(input: String): Long = {

    input
      .split(",")
      .map({ case s"$lo-$hi" => (lo.toLong, hi.toLong) })
      .map({ case (a, b) =>
        (a to b)
          .filter(i => {
            val is = i.toString()
            val len = is.length()
            (2 to len)
              .filter(x => len % x == 0)
              .exists(div => {
                val divlen = len / div
                val first = is.slice(0, divlen)
                def matches(s: String): Boolean = {
                  s.length() == 0 || s.slice(0, divlen) == first &&
                  matches(s.slice(divlen, s.length))
                }
                matches(is)
              })
          })
          .foldLeft(0L)(_ + _)
      })
      .sum
  }

}
