import scala.annotation.tailrec
import scala.io.Source

val lines = Source.fromResource("day01.txt").getLines().toList

val partOneSolution = lines
  .map(line =>
    val digits = line.filter(_.isDigit)
    s"${digits.head}${digits.last}".toInt
  )
  .sum

println(partOneSolution)

def replacements = Map(
  "one"   -> 1,
  "two"   -> 2,
  "three" -> 3,
  "four"  -> 4,
  "five"  -> 5,
  "six"   -> 6,
  "seven" -> 7,
  "eight" -> 8,
  "nine"  -> 9
)

@tailrec
def findFirstDigit(s: String): Option[Int] = {
  if s.isEmpty then return None

  val firstChar = s(0)
  if firstChar.isDigit then return Some(firstChar.asDigit)

  val numberString = replacements.keys.find(s.startsWith)
  if numberString.isDefined then return Some(replacements(numberString.get))

  findFirstDigit(s.substring(1))
}

@tailrec
def findLastDigit(s: String): Option[Int] = {
  if s.isEmpty then return None

  val lastChar = s(s.length - 1)
  if lastChar.isDigit then return Some(lastChar.asDigit)

  val numberString = replacements.keys.find(s.endsWith)
  if numberString.isDefined then return Some(replacements(numberString.get))

  findLastDigit(s.substring(0, s.length - 1))
}

val partTwoSolution = lines
  .map(line => s"${findFirstDigit(line).get}${findLastDigit(line).get}".toInt)
  .sum

println(partTwoSolution)
