import scala.annotation.tailrec
import scala.io.BufferedSource

def second(fn: String, ver: Int = 2): Int = {
  scala.io.Source.fromFile(fn)
    .getLines().foldLeft(0) { (acc, line) => 

      val list = (for {
        matches <- ("(\\d+)|(\\w+)".r).findAllMatchIn(line)
      } yield matches.group(0))
      .toList
      ver match
        case 1 => acc + checkLimits(list.drop(2), list(1).toInt)
        case 2 => {
          acc + getHighestOfEach(list.drop(2)).reduce(_*_)
        }
    }
}

@tailrec
def checkLimits(l: List[String], value: Int, limits: Map[String, Int] = Map("blue" -> 14, "green" -> 13, "red" -> 12)): Int = {
  l match {
    case Nil => value
    case num :: colour :: next => if (num.toInt > limits(colour)) 0 else checkLimits(next, value)
    case _ => throw new IllegalArgumentException("ugyldig \"farge\"")
    }
}

@tailrec
def getHighestOfEach(l: List[String], highestOfEach: Map[String,Int] = Map(("blue" -> 0), ("red" -> 0), ("green" -> 0))): List[Int] = {
   l match {
    case Nil => highestOfEach.values.toList
    case num :: colour :: next => 
      if (highestOfEach(colour) < num.toInt) getHighestOfEach(next, highestOfEach.updated(colour, num.toInt))
      else getHighestOfEach(next, highestOfEach)
    case _ => List.empty // Not implemented
  }
}

println(
s"""
          |Advent of Code 2023 - Day 2
          |  Part 1: ${second("input/2_1", 1)}
          |  Part 2: ${second("input/2_1", 2)}
""".stripMargin
)