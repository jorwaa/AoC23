import scala.io.BufferedSource

case class Matches(index: Int, symbols: List[SymbolMatch], numbers: List[NumberMatch]) // penere med en case class :)
case class NumberMatch(start: Int, end: Int, matched: Int)
case class SymbolMatch(index: Int, symbol: String)

def third(fn: String, ver: Int): Int = {
  val lines: List[String] = scala.io.Source.fromFile(fn).getLines().toList
  val padding: String = ".".repeat(lines(0).length)
  val paddedLines: List[String] = padding +: lines :+ padding
  ver match
    case 1 => silver(paddedLines)
    case 2 => gold(paddedLines)
    case _ => throw new IllegalArgumentException("Version not yet implemented")
}

def gold(lines: List[String]): Int = {
  val reducedLines: List[Matches] = adjacentNumbers(lines).map {case Matches(index, symbols, numbers) => {
    Matches(index, symbols.filter(s => s.symbol == "*"), numbers)
  }}
  reducedLines.map(allMatches => {
  val lineNum = allMatches.index
  allMatches.symbols.map(symbol => {
    val i = symbol.index
    reducedLines.slice(lineNum - 1, lineNum + 2).map {case Matches(_, _, numbers) => {
    numbers.dropWhile(number => number.end < i)
    .reverse.dropWhile(number => number.start > i+1)
    }}.flatten
    match
      case first :: second :: Nil => first.matched * second.matched
      case Nil => 0
      case any => 0
  }).sum
    }).sum
}

def silver(lines: List[String]) = {
  val reducedLines: List[Matches] = adjacentNumbers(lines)
  //v2:
  reducedLines.map(allMatches => {
    val lineNum = allMatches.index
    allMatches.numbers.map(num => {
      reducedLines.slice(lineNum - 1, lineNum + 2).map {case Matches(_, symbols, _) => {
        symbols.dropWhile(symbol => symbol.index < num.start-1 || symbol.index > num.end)
      }}.flatten
      match
        case Nil => 0
        case any => num.matched
    }).sum
  }).sum
}

def adjacentNumbers(lines: List[String]): List[Matches] = lines.zipWithIndex.map((line, index) => {
      val symbols = s"([^\\d\\.\\s])".r.findAllMatchIn(line)
      val numbers = s"(\\d+)".r.findAllMatchIn(line)
      Matches(
        index,
        symbols.map { case s => SymbolMatch(s.start, s.matched)}.toList,
        numbers.map { case m => NumberMatch(m.start, m.end, m.matched.toInt)}.toList
      )
  })
println(
s"""
          |Advent of Code 2023 - Day 3
          |  Part 1: ${third("input/3_1", 1)}
          |  Part 2: ${third("input/3_1", 2)}
""".stripMargin
)