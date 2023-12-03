import scala.annotation.tailrec
import scala.io.Source

val lines = Source.fromResource("day03.txt").getLines().toList

type Position = Int
case class NumberInfo(number: Int, start: Position, end: Position)

def parseSymbols(line: String): List[Position] =
  val notDigitOrDot = "[^.\\d]".r
  notDigitOrDot.findAllMatchIn(line).map(m => m.start).toList

def parseNumbers(line: String): List[NumberInfo] =
  "\\d+".r
    .findAllMatchIn(line)
    .map(m => NumberInfo(m.matched.toInt, m.start, m.end))
    .toList

def isNumberAdjacentToSymbol(
    number: NumberInfo,
    symbols: List[Position]
): Boolean =
  symbols.exists(pos => pos >= (number.start - 1) && pos <= number.end)

def sumPartNumbersInLine(
    numbers: List[NumberInfo],
    symbols: List[Position]
): Int =
  numbers
    .filter(isNumberAdjacentToSymbol(_, symbols))
    .map(_.number)
    .sum

@tailrec
def sumPartNumbers(
    lines: List[String],
    accumulatedSum: Int
): Int =
  lines match
    case remainingLines if remainingLines.length >= 3 =>
      // Look at the first three lines of the remaining file.
      // Get numbers and their positions from the middle line
      // and symbol positions from all three lines.
      // Then find numbers that are valid "part numbers" and sum them up.
      val numbers = parseNumbers(remainingLines(1))
      val symbolPositions = parseSymbols(remainingLines(0))
        ++ parseSymbols(remainingLines(1))
        ++ parseSymbols(remainingLines(2))
      val lineResult = sumPartNumbersInLine(numbers, symbolPositions)

      // Recurse with one less line, accumulate the result from the current iteration
      sumPartNumbers(remainingLines.drop(1), accumulatedSum + lineResult)

    case remainingLines if remainingLines.length == 2 =>
      // Terminal case, only 2 lines remaining. Parse numbers from the bottom line.
      val numbers = parseNumbers(remainingLines(1))
      val symbolPositions =
        parseSymbols(remainingLines(0)) ++ parseSymbols(remainingLines(1))
      val lineResult = sumPartNumbersInLine(numbers, symbolPositions)

      accumulatedSum + lineResult // Terminate with the final sum

    case _ => accumulatedSum // default case to keep compiler happy

// Pad the start of the file with an extra empty line, so that numbers from the first actual line are read.
val emptyLine   = "." * lines.head.length
val paddedLines = emptyLine :: lines
val solutionOne = sumPartNumbers(paddedLines, 0)

// ---------------------------------------------------------------------------------------

def parseGearPositions(line: String): List[Position] =
  "\\*".r.findAllMatchIn(line).map(m => m.start).toList

def listNumbersAdjacentToGear(
    gear: Position,
    numbers: List[NumberInfo]
): List[NumberInfo] =
  numbers.filter(n => isNumberAdjacentToSymbol(n, List(gear)))

def sumGearRatiosInLine(
    gears: List[Position],
    numbers: List[NumberInfo]
): Int =
  gears
    .map(listNumbersAdjacentToGear(_, numbers))
    .filter(_.length == 2) // a valid gear has exactly two adjacent numbers
    .map(n => n(0).number * n(1).number)
    .sum

@tailrec
def sumGearRatios(
    lines: List[String],
    accumulatedSum: Int
): Int =
  lines match
    case remainingLines if remainingLines.length >= 3 =>
      // Again, look at the top three lines. This time, get gear positions
      // from the middle line and number positions from all three.
      // Then find valid gear ratios and sum them up.
      val gearPositions = parseGearPositions(remainingLines(1))
      val numberPositions = parseNumbers(remainingLines(0))
        ++ parseNumbers(remainingLines(1))
        ++ parseNumbers(remainingLines(2))
      val lineResult = sumGearRatiosInLine(gearPositions, numberPositions)

      sumGearRatios(remainingLines.drop(1), accumulatedSum + lineResult)

    case remainingLines if remainingLines.length == 2 =>
      // Terminal case with two lines remaining, as before
      val gearPositions = parseGearPositions(remainingLines(1))
      val numberPositions =
        parseNumbers(remainingLines(0)) ++ parseNumbers(remainingLines(1))
      val lineResult = sumGearRatiosInLine(gearPositions, numberPositions)

      accumulatedSum + lineResult // Terminate with final result

    case _ => accumulatedSum // default case to keep compiler happy

val solutionTwo = sumGearRatios(paddedLines, 0)
