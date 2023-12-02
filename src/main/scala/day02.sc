import scala.io.Source
import scala.util.matching.Regex

val lines = Source.fromResource("day02.txt").getLines().toList

// Checks whether a given draw ("12 red", "42 blue" etc.) contains a valid amount of cubes
def isValidDraw(draw: String): Boolean = {
  val split  = draw.split(" ")
  val amount = split(0).toInt
  val color  = split(1)

  color match
    case "red"   => amount <= 12
    case "green" => amount <= 13
    case "blue"  => amount <= 14
    case _       => false
}

// Checks that all recorded draws within a game are valid
def isValidGame(line: String): Boolean = {
  "[0-9]+ (red|green|blue)".r
    .findAllMatchIn(line)
    .forall(m => isValidDraw(m.matched))
}

// "Game 123: ..." -> 123
def parseGameId(line: String): Int = {
  line.stripPrefix("Game ").split(":")(0).toInt
}

val solutionOne = lines
  .filter(isValidGame)
  .map(parseGameId)
  .sum

// "42 red" -> 42
def parseNumberOfCubes(draw: String): Int = draw.split(" ")(0).toInt

// Return the max amount of cubes of a given color appearing in the line
def getMinRequiredCubes(color: String, line: String): Int = {
  s"[0-9]+ ${color}".r
    .findAllMatchIn(line)
    .map(m => parseNumberOfCubes(m.matched))
    .max
}

def getPowerOfCubes(line: String): Int = {
  getMinRequiredCubes("red", line) *
    getMinRequiredCubes("green", line) *
    getMinRequiredCubes("blue", line)
}

val solutionTwo = lines.map(getPowerOfCubes).sum
