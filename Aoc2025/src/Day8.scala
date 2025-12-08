import Aoc2025.macros.Macros.logged

import scala.annotation.tailrec
import scala.util.chaining.scalaUtilChainingOps

@scala.annotation.experimental
object Day8 {

  val day = 8
  def main(args: Array[String]): Unit = {
    val test = """162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689"""
    val input = os.read(os.pwd / "input" / s"$day.txt")
    println(s"Part 1 test: ${part1(test, 10)}")
    println(s"Part 1: ${part1(input, 1000)}")
    println(s"Part 2 test: ${part2(test)}")
    println(s"Part 2: ${part2(input)}")
  }
  extension (i: Long) {
    def sq: Long = i * i
  }
  case class Pos(x: Long, y: Long, z: Long) {
    def dist(p: Pos): Long =
      (p.x - x).sq + (p.y - y).sq + (p.z - z).sq
  }

  class UnionFind(n: Int) {
    val rep_ = (0 until n).toArray
    val size = Array.fill(n)(1)

    def union(a: Int, b: Int): Int = {
      val ra = find(a)
      val rb = find(b)
      if (ra != rb) {
        val sizea = size(ra)
        val sizeb = size(rb)
        val (from, to) = if (sizea > sizeb) {
          (rb, ra)
        } else (ra, rb)
        size(to) += size(from)
        rep_(from) = to
        to
      } else {
        ra
      }
    }
    def find(a: Int): Int = {
      val r = rep_(a)
      if (a == r)
        r
      else {
        rep_(a) = find(r)
        rep_(a)
      }
    }
  }
  def part1(input: String, toConnect: Int): Int = {
    val boxes = input
      .split("\n")
      .map({ case s"$x,$y,$z" => Pos(x.toLong, y.toLong, z.toLong) })
    val n = boxes.length
    val pairs =
      (0 until n)
        .flatMap(ind => ((ind + 1) until n).map(b2 => (ind, b2)))
        .sortBy({ case (p1, p2) => boxes(p1).dist(boxes(p2)) })
    val uf = new UnionFind(n)
    pairs.slice(0, toConnect).foreach({ case (a, b) => uf.union(a, b) })
    (0 until n)
      .map(uf.find)
      .groupBy(identity)
      .map(_._2.size)
      .toVector
      .sortBy(-_)
      .slice(0, 3)
      .product
  }

  def part2(input: String): Long = {
    val boxes = input
      .split("\n")
      .map({ case s"$x,$y,$z" => Pos(x.toLong, y.toLong, z.toLong) })
    val n = boxes.length
    val pairs =
      (0 until n)
        .flatMap(ind => ((ind + 1) until n).map(b2 => (ind, b2)))
        .sortBy({ case (p1, p2) => boxes(p1).dist(boxes(p2)) })
    val uf = new UnionFind(n)
    @tailrec
    def findAnswer(connected: Int = 0, ind: Int = 0): Long = {
      val (a, b) = pairs(ind)
      if (uf.find(a) != uf.find(b)) {
        uf.union(a, b)
        if (connected == n - 2) {
          boxes(a).x * boxes(b).x
        } else
          findAnswer(connected + 1, ind + 1)
      } else {
        findAnswer(connected, ind + 1)
      }

    }
    findAnswer()
  }
}
