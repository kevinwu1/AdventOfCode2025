object Setup {

  def main(args: Array[String]): Unit = {
    generateDay(11)
  }

  def generateInputFiles(): Unit = {
    (1 to 12).foreach(day => {
      val fname = day + ".txt"
      os.write(os.pwd / "input" / fname, "")
    })
  }

  def dayTemplate = (day: Int) => s"""import Aoc2025.macros.Macros.logged

import scala.annotation.tailrec
import scala.util.chaining.scalaUtilChainingOps

@scala.annotation.experimental
object Day$day {

  val day = $day
  def main(args: Array[String]): Unit = {
    val test = \"\"\"
\"\"\"
    val input = os.read(os.pwd / "input" / s"$$day.txt")
    println(s"Part 1 test: $${part1(test)}")
    println(s"Part 1: $${part1(input)}")
    println(s"Part 2 test: $${part2(test)}")
    println(s"Part 2: $${part2(input)}")
  }
  type Pos = (Int, Int)
  extension (p: Pos) {
    def r: Int = p._1
    def c: Int = p._2
    def +(o: Pos): Pos =
      (r + o.r, c + o.c)
    def unary_- : Pos =
      (-r, -c)
    def -(o: Pos): Pos =
      p + -o
    def manhattan(o: Pos): Int =
      Math.abs(o.r - p.r) + Math.abs(o.c - p.c)
  }
  type Grid[T] = Array[Array[T]]
  extension [T](g: Grid[T]) {
    def apply(p: Pos): T = g(p.r)(p.c)
    def update(p: Pos, t: T) = g(p.r)(p.c) = t
    def width: Int = g(0).length
    def height: Int = g.length
    def positions: IndexedSeq[Pos] = for {
      h <- 0 until height;
      w <- 0 until width
    } yield (h, w)
  }
  def inBounds[T](p: Pos)(using grid: Grid[T]): Boolean = {
    val (y, x) = p
    0 <= y && y < grid.height &&
    0 <= x && x < grid.width
  }
  def adjacent[T](p: Pos)(using grid: Grid[T]): Vector[Pos] = {
    val (y, x) = p
    (-1 to 1)
      .flatMap(dy => {
        (-1 to 1).map(dx => (dy, dx))
      })
      .filter({ case (dy, dx) => dx != 0 || dy != 0 })
      .map(_ + p)
      .filter(inBounds(_))
      .toVector
  }
  def part1(input: String): Long = {
    given grid: Grid[String] = input
      .split("\\n")
      .map(_.split(""))
    0L
  }

  def part2(input: String): Long = {
    given grid: Grid[String] = input
      .split("\\n")
      .map(_.split(""))

    0L
  }
}

"""
  def generateDay(day: Int): Unit = {
    os.write(os.pwd / "Aoc2025" / "src" / s"Day$day.scala", dayTemplate(day))
  }
}
