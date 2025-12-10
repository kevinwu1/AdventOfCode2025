import Aoc2025.macros.Macros.logged
import Day4.adjacent

import scala.annotation.tailrec
import scala.util.chaining.scalaUtilChainingOps

@scala.annotation.experimental
object Day9 {
  val day = 9
  def main(args: Array[String]): Unit = {
    val test = """7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3"""
    val input = os.read(os.pwd / "input" / s"$day.txt")
    // println(s"Part 1 test: ${part1(test)}")
    // println(s"Part 1: ${part1(input)}")
    println(s"Part 2 test: ${part2(test)}")
    println(s"Part 2: ${part2(input)}")
  }
  case class Point(x: Long, y: Long) {}
  def area(p: Point, q: Point): Long = {
    (p.x - q.x + 1).abs * (p.y - q.y + 1).abs
  }
  def area(p: Pos, q: Pos): Long = {
    ((p.r - q.r).abs + 1).toLong * ((p.c - q.c).abs + 1).toLong
  }
  def part1(input: String): Long = {
    val points =
      input.split("\n").map({ case s"$x,$y" => Point(x.toLong, y.toLong) })
    points.flatMap(p => points.map(q => area(p, q))).max
  }
  type Pos = (Int, Int)
  enum Direction(v: Pos) {
    case Up extends Direction((-1, 0))
    case Down extends Direction((1, 0))
    case Left extends Direction((0, -1))
    case Right extends Direction((0, 1))
  }
  import Direction.*
  extension (p: Pos) {
    def r: Int = p._1
    def c: Int = p._2
    def +(o: Pos): Pos =
      (r + o.r, c + o.c)
    def unary_- : Pos =
      (-r, -c)
    def -(o: Pos): Pos =
      p + -o
    def determinant(o: Pos): Int = { r * o.c - c * o.r }
    def direction: Direction = {
      assert(r == 0 || c == 0)
      if (c > 0) Right
      else if (c < 0) Left
      else if (r > 0) Down
      else Up
    }

  }

  def hlineInter(
      h1: Pos,
      h2: Pos,
      vseg: (Pos, Pos)
  ): Boolean = {
    val r = h1.r
    val clo = Math.min(h1.c, h2.c)
    val chi = Math.max(h1.c, h2.c)

    val rlo = Math.min(vseg._1.r, vseg._2.r)
    val rhi = Math.max(vseg._1.r, vseg._2.r)
    val segc = vseg._1.c
    (rlo < r && r < rhi) && (clo < segc && segc < chi)

  }
  def vlineInter(
      v1: Pos,
      v2: Pos,
      hseg: (Pos, Pos)
  ): Boolean = {
    val c = v1.c
    val rlo = Math.min(v1.r, v2.r)
    val rhi = Math.max(v1.r, v2.r)

    val clo = Math.min(hseg._1.c, hseg._2.c)
    val chi = Math.max(hseg._1.c, hseg._2.c)
    val segr = hseg._1.r
    ((clo < c && c < chi) && (rlo < segr && segr < rhi))

  }
  type Segment = (Pos, Pos)
  type Turn = (Segment, Segment)
  extension (s: Segment) {
    def delta: Pos = s._2 - s._1
  }
  extension (turn: Turn) {
    def turnType: (Direction, Direction) = {
      val (s1, s2) = turn
      (s1.delta.direction, s2.delta.direction)
    }
  }

  def part2(input: String): Long = {

    val points =
      input.split("\n").map({ case s"$x,$y" => (y.toInt, x.toInt) })

    val segments = (points :+ points.head)
      .sliding(2)
      .map({ case Array(a, b) => (a, b) })
      .toVector

    val turns = (segments :+ segments.head)
      .sliding(2)
      .map({ case Vector(s1, s2) =>
        s1._2 -> (s1, s2)
      })
      .toMap
    val concaves = turns.toVector
      .flatMap({ case corner -> (s1, s2) =>
        val v1 = s1._2 - s1._1
        val v2 = s2._2 - s2._1
        if (v1.determinant(v2) > 0) Some(s1._2) else None
      })
      .toSet
    val convexes = points.toSet -- concaves
    val (vsegs, hsegs) = segments.partition({ case (a, b) => a.c == b.c })

    points
      .flatMap(p => points.map(q => (p, q)))
      .filter({
        case (p, q) => // assuming answer wont be a 1-wide or 1-tall rect
          p.r != q.r && p.c != q.c
      })
      .filter({ case (p, q) =>
        val c1 =
          (p.r, q.c) // p and q are opposite corners. c1 and c2 are the other 2 corners of the rect
        val c2 = (q.r, p.c)
        val corners = Vector(p, c1, q, c2)
        (corners :+ corners.head)
          .sliding(2) // each of the 4 edges of the rectangle
          .forall({ case Vector(a, b) =>
            if (a.r == b.r) { // if it's a vertical edge
              val r = a.r
              val clo = Math.min(a.c, b.c)
              val chi = Math.max(a.c, b.c)
              !vsegs.exists(vs =>
                hlineInter(a, b, vs)
              ) && // make sure there's no segment that intersects the edge.
              // also, there cannot be any convex corner on the edge, but there sometimes can be a convex corner on the corner of the rect
              !convexes.exists(p => p.r == r && clo < p.c && p.c < chi)
            } else {
              assert(a.c == b.c)
              val c = a.c
              val rlo = Math.min(a.r, b.r)
              val rhi = Math.max(a.r, b.r)
              !hsegs.exists(hs => vlineInter(a, b, hs)) &&
              !convexes.exists(p => {
                p.c == c && rlo < p.r && p.r < rhi
              })
            }
          })
        && {

          val Vector(topleft, topright, botleft, botright) =
            corners.sorted
          val allowedTurnTypes = Map(
            topleft -> Vector((Up, Right), (Left, Down)),
            topright -> Vector((Right, Down), (Up, Left)),
            botleft -> Vector((Down, Right), (Left, Up)),
            botright -> Vector((Right, Up), (Down, Left))
          )
          convexes
            .intersect(corners.toSet)
            .forall(corner => {
              allowedTurnTypes(corner).contains(turns(corner).turnType)
            })
        }
      })
      .map({ case (p, q) => area(p, q) })
      .max

  }
}
