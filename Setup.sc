//> using scalacOptions -rewrite -no-indent
//> using dep com.lihaoyi::os-lib:0.11.6
object Setup {

  def main(args: Array[String]): Unit = {
    generateDay(3)
  }

  def generateInputFiles(): Unit = {
    (1 to 12).foreach(day => {
      val fname = day + ".txt"
      os.write(os.pwd / "input" / fname, "")
    })
  }

  def dayTemplate = (day: Int) => s"""//> using dep com.lihaoyi::os-lib:0.11.6

import scala.util.chaining.scalaUtilChainingOps

object Day$day {

	val day = $day
	def main(args: Array[String]): Unit = {
		val test = \"\"\"\"\"\"
		val input = os.read(os.pwd / "input" / s"$$day.txt")
		println(s"Part 1 test: $${part1(test)}")
		println(s"Part 1: $${part1(input)}")
		println(s"Part 2 test: $${part2(test)}")
		println(s"Part 2: $${part2(input)}")
	}

	def part1(input: String): String = {
		input
			.split("\\n")
			.toString()
	}
	def part2(input: String): String = {
		input
			.split("\\n")
			.toString()
	}
	extension (i: Int) def %%(m: Int): Int = {
		val r = i % m
		if (r < 0)
			r + m
		else r
	}
}
"""
  def generateDay(day: Int): Unit = {
    os.write(os.pwd / s"Day$day.sc", dayTemplate(day))
  }
}
