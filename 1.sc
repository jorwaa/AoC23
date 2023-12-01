import scala.util.matching.Regex
import scala.io.BufferedSource

def readArtsyCalibrationFile(path: String, verFlag: Int): Int = 
  val file: BufferedSource = scala.io.Source.fromFile(path)

  file.getLines().foldLeft(0) { (acc, line) => 
    verFlag match
      case 1 => acc + firstVersion(line)
      case 2 => acc + secondVersion(line)
      case _ => throw new IllegalArgumentException(s"Version $verFlag not yet implemented")
  }
def firstVersion(line: String) = 
  val firstDigit =  line.find{c => c.isDigit}.get
  val lastDigit =  line.reverse.find{c => c.isDigit}.get
  s"$firstDigit$lastDigit".toInt


def secondVersion(line: String): Int = 
  val numbersMap = Map[String, Int](
    ("1" -> 1),
    ("2" -> 2),
    ("3" -> 3),
    ("4" -> 4),
    ("5" -> 5),
    ("6" -> 6),
    ("7" -> 7),
    ("8" -> 8),
    ("9" -> 9),
    ("0" -> 0),
    ("one" -> 1), 
    ("two" -> 2),
    ("three" -> 3),
    ("four" -> 4),
    ("five" -> 5),
    ("six" -> 6),
    ("seven" -> 7),
    ("eight" -> 8),
    ("nine" -> 9)
  )
  val regexMatcher = s"${numbersMap.keys.mkString("(","|",")")}".r

  val numbersFound: Iterator[String] = for {
    numbers <- regexMatcher.findAllMatchIn(line)
  } yield numbers.group(0)
  
  val numbersList = numbersFound
    .toList
    .map(numbersMap(_))

  s"${numbersList.head}${numbersList.last}".toInt




println(
s"""
          |Advent of Code 2023 - Day 1
          |  Part 1: ${readArtsyCalibrationFile("input/1_1", 1)}
          |  Part 2: ${readArtsyCalibrationFile("input/1_1", 2)}
""".stripMargin
)