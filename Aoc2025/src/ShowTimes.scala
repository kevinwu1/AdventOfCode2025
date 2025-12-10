import upickle.default.*

import java.time.Duration
import java.time.Instant
import java.time.LocalDateTime
import java.time.ZoneId
import java.time.ZonedDateTime
import scala.util.chaining.scalaUtilChainingOps

object ShowTimes {
  def main(args: Array[String]): Unit = {
    showTimes()
  }
  case class Completion(star_index: Int, get_star_ts: Int) derives ReadWriter
  case class Member(
      name: String,
      stars: Int,
      id: Int,
      completion_day_level: Map[String, Map[String, Completion]]
  ) derives ReadWriter
  def showTimes(): Unit = {
    val json = ujson.read(os.read(os.pwd / "leaderboard.json"))
    val members = json
      .obj("members")
      .obj
      .values
      .map(upickle.default.read[Member](_))
      .toVector
    val days = 10
    (1 to days).map(day => {
      val start = ZonedDateTime
        .of(
          2025,
          12,
          day,
          21,
          0,
          0,
          0,
          ZoneId.of("America/Los_Angeles")
        )
        .minusDays(1)
      Vector(1, 2).map(part => {
        println(s"$day-$part")
        members
          .collect({
            case Member(name, _, _, compl)
                if compl.contains(day.toString()) && compl(day.toString())
                  .contains(part.toString()) =>
              (
                name,
                compl(day.toString())(part.toString()).get_star_ts
                  .pipe(ts =>
                    Duration
                      .between(
                        start.toInstant(),
                        Instant.ofEpochSecond(ts)
                      )
                      .toMinutes()
                  )
              )
          })
          .sortBy(x => x._2)
          .slice(0, 10)
          .map(x => "  " + x)
          .mkString("\n")
          .pipe(println(_))
      })
    })
  }
}
