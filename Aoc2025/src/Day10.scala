import Aoc2025.macros.Macros.logged
import org.chocosolver.solver.Model
import org.chocosolver.util.criteria.Criterion
import org.ejml.data.DMatrixRMaj
import org.ejml.dense.row.misc.RrefGaussJordanRowPivot_DDRM
import org.ejml.simple.SimpleMatrix

import scala.annotation.tailrec
import scala.util.chaining.scalaUtilChainingOps
@scala.annotation.experimental
object Day10 {

  val day = 10
  def main(args: Array[String]): Unit = {
    val test = """[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"""
    val input = os.read(os.pwd / "input" / s"$day.txt")
    println(s"Part 1 test: ${part1(test)}")
    println(s"Part 1: ${part1(input)}")
    println(s"Part 2 test: ${part2(test)}")
    println(s"Part 2: ${part2(input)}")
  }
  def part1(input: String): Long = {
    input
      .split("\n")
      .map({ case s"[$targetsS] $buttonsS {$curliesS}" =>
        // println(s"$targetsS and $buttonsS and $curliesS")
        val target = targetsS
          .split("")
          .map({
            case "#" => 1
            case _   => 0
          })
          .reverse
          .foldLeft(0)({ case (acc, bit) => (acc << 1) + bit })
        // println(Integer.toBinaryString(target))
        val buttons = buttonsS
          .split(" ")
          .map({ case s"($inds)" =>
            inds.split(",").map(_.toInt).map(1 << _).reduce(_ | _)
          })
          .toVector
        val n = buttons.length
        def press(i: Int): Int = {
          var ans = 0
          (0 until n).foreach(ind =>
            if ((i & (1 << ind)) != 0)
              ans ^= buttons(ind)
          )
          ans
        }
        (0 until (1 << n))
          .map(it =>
            if (press(it) == target)
              Integer.bitCount(it)
            else -1
          )
          .filter(_ != -1)
          .min
      })
      .sum

  }
  def part2(input: String): Long = {
    input
      .split("\n")
      .map({ case s"[$_] $buttonsS {$targetsS}" =>
        val targets = targetsS.split(",").map(_.toInt).toVector
        val n = targets.size
        val buttons = buttonsS
          .split(" ")
          .map({ case s"($inds)" =>
            val v = inds.split(",").map(_.toInt)
            (0 until n).map(ind => if (v.contains(ind)) 1 else 0).toVector
          })
          .toVector
        val mat = {
          val matTemp = (0 until n)
            .map(row => {
              (0 until buttons.length)
                .map(col => {
                  buttons(col)(row).toDouble
                })
                .toArray :+ targets(row).toDouble
            })
            .toArray
            .pipe(x => new DMatrixRMaj(x))
          new RrefGaussJordanRowPivot_DDRM()
            .reduce(matTemp, matTemp.numCols - 1)
          matTemp
        }
        solve(cleanup(mat.get2DData()))

      })
      .sum
  }

  val prec = 1e-7
  def isImprecise(dataD: Array[Array[Double]]): Boolean =
    dataD.exists(_.exists(x => {
      val rounded = x.round.toInt
      (x - rounded).abs > prec
    }))
  def matToInt(dataD: Array[Array[Double]]): Array[Array[Int]] =
    dataD.map(_.map(x => {
      x.round.toInt
    }))
  def cleanup(dataD: Array[Array[Double]]): Array[Array[Int]] = {
    if (!isImprecise(dataD))
      matToInt(dataD)
    else {
      @tailrec
      def findLowestMult(it: Int = 2): Array[Array[Int]] = {
        val multiplied = dataD.map(_.map(_ * it))
        if (!isImprecise(multiplied))
          matToInt(multiplied)
        else
          findLowestMult(it + 1)
      }
      findLowestMult()
    }
  }
  def solve(data: Array[Array[Int]]): Long = {
    val n = data.head.length - 1
    val targets = data.map(_.last)
    val model = new Model()
    val vars = (0 until n)
      .map({ case x =>
        model.intVar("a" + x, 0, 200)
      })
    data.foreach(row => {
      val res = row.last
      val tosum = row
        .dropRight(1)
        .zipWithIndex
        .flatMap({ case (co, ind) =>
          if (co == 0)
            None
          else
            Some(model.mul(vars(ind), co))
        })
        .toArray
      if (tosum.nonEmpty)
        model
          .sum(
            tosum,
            "=",
            res
          )
          .post()
    })
    val total = model.intVar("total", 0, 1000);
    model.sum(vars.toArray, "=", total).post()
    model.setObjective(Model.MINIMIZE, total)
    val solver = model.getSolver()

    var best = 1000000
    while (solver.solve()) {
      best = best min total.getValue();
    }
    best
  }
}
