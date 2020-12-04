package jgogstad.day4

import cats.data.NonEmptyList
import cats.effect.{Blocker, ExitCode, IO, IOApp}
import cats.syntax.all._
import fs2.Stream
import io.odin.{consoleLogger, Logger}
import jgogstad.utils.FileIo

import scala.util.Try

object Task extends IOApp {
  val log: Logger[IO] = consoleLogger()
  def task1Validate(s: String): Boolean =
    NonEmptyList.of("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid").reduceMap(s.contains)(_ && _)

  def program(f: String => Boolean): IO[Int] = Blocker[IO].use { blocker =>
    FileIo
      .contentsOf[IO]("day4/input.txt", blocker)
      .groupAdjacentBy(_.nonEmpty)
      .mapFilter(t => t._1.guard[Option].as(t._2.mkString_(" ")))
      .filter(f)
      .foldMap(_ => 1)
      .compile
      .lastOrError
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val runTask1 = program(task1Validate).flatTap(i => log.info(i.show)).as(ExitCode.Success)
    val runTask2 = program(task2Validate).flatTap(i => log.info(i.show)).as(ExitCode.Success)

    runTask2
  }

  def task2Validate(s: String): Boolean = {
    val byr = "(?:^|\\s)byr:(\\d{4})(?:\\s|$)".r
      .findFirstMatchIn(s)
      .flatMap(m => Try(m.group(1).toInt).toOption)
      .fold(false)(i => i >= 1920 && i <= 2002)

    val iyr = "(?:^|\\s)iyr:(\\d{4})(?:\\s|$)".r
      .findFirstMatchIn(s)
      .flatMap(m => Try(m.group(1).toInt).toOption)
      .fold(false)(i => i >= 2010 && i <= 2020)

    val eyr = "(?:^|\\s)eyr:(\\d{4})(?:\\s|$)".r
      .findFirstMatchIn(s)
      .flatMap(m => Try(m.group(1).toInt).toOption)
      .fold(false)(i => i >= 2020 && i <= 2030)

    val hgt = "(?:^|\\s)hgt:(\\d+)(cm|in)(?:\\s|$)".r
      .findFirstMatchIn(s)
      .map(m => Try(m.group(1).toInt) -> Try(m.group(2)))
      .flatMap(_.tupled.toOption)
      .fold(false) {
        case (height, "cm") => height >= 150 && height <= 193
        case (height, "in") => height >= 59 && height <= 76
        case _ => false
      }

    val hcl = "(?:^|\\s)hcl:#[0-9a-f]{6}(?:\\s|$)".r.findFirstIn(s).isDefined
    val ecl = "(?:^|\\s)ecl:(amb|blu|brn|gry|grn|hzl|oth)(?:\\s|$)".r.findFirstIn(s).isDefined
    val pid = "(?:^|\\s)pid:\\d{9}(?:\\s|$)".r.findFirstIn(s).isDefined

    byr && iyr && eyr && hgt && hcl && ecl && pid
  }

  private val task1ExampleData: Stream[IO, String] = {
    val data = """ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
                 |byr:1937 iyr:2017 cid:147 hgt:183cm
                 |
                 |iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
                 |hcl:#cfa07d byr:1929
                 |
                 |hcl:#ae17e1 iyr:2013
                 |eyr:2024
                 |ecl:brn pid:760753108 byr:1931
                 |hgt:179cm
                 |
                 |hcl:#cfa07d eyr:2025 pid:166559648
                 |iyr:2011 ecl:brn hgt:59in""".stripMargin
    Stream.emits(data.split("\n")).covary[IO]
  }

  private val task2Invalid: Stream[IO, String] = {
    val data = """eyr:1972 cid:100
                 |hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926
                 |
                 |iyr:2019
                 |hcl:#602927 eyr:1967 hgt:170cm
                 |ecl:grn pid:012533040 byr:1946
                 |
                 |hcl:dab227 iyr:2012
                 |ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277
                 |
                 |hgt:59cm ecl:zzz
                 |eyr:2038 hcl:74454a iyr:2023
                 |pid:3556412378 byr:2007""".stripMargin
    Stream.emits(data.split("\n"))
  }

  private val task2Valid: Stream[IO, String] = {
    val data = """pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
                 |hcl:#623a2f
                 |
                 |eyr:2029 ecl:blu cid:129 byr:1989
                 |iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm
                 |
                 |hcl:#888785
                 |hgt:164cm byr:2001 iyr:2015 cid:88
                 |pid:545766238 ecl:hzl
                 |eyr:2022
                 |
                 |iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719""".stripMargin
    Stream.emits(data.split("\n"))
  }
}
