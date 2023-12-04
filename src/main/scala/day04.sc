import scala.annotation.tailrec
import scala.io.Source

val lines = Source.fromResource("day04.txt").getLines().toList

case class Scratchcard(winningNumbers: List[Int], playedNumbers: List[Int])

def parseNumbers(numberString: String): List[Int] =
  "\\d+".r.findAllMatchIn(numberString).map(m => m.matched.toInt).toList

def parseScratchcard(line: String): Scratchcard =
  val numbers = line.split(":")(1).split("\\|")
  Scratchcard(parseNumbers(numbers(0)), parseNumbers(numbers(1)))

def calculatePoints(card: Scratchcard): Int =
  card.playedNumbers.foldLeft(0)((currentPoints: Int, number: Int) =>
    number match
      case n if card.winningNumbers.contains(n) =>
        if currentPoints == 0 then 1 else currentPoints * 2
      case _ => currentPoints
  )

val solutionOne = lines
  .map(parseScratchcard)
  .map(calculatePoints)
  .sum

// ---------------------------------------------------------------------

case class CardStack(card: Scratchcard, copies: Int)

def getInitialStack(line: String): CardStack =
  CardStack(parseScratchcard(line), 1) // start with one copy of each card

def addCopies(stack: CardStack, amountToAdd: Int): CardStack =
  CardStack(stack.card, stack.copies + amountToAdd)

def addBonusCards(
    stacks: List[CardStack],
    amountToAdd: Int,
    numberOfMatches: Int
): List[CardStack] =
  stacks.zipWithIndex
    .map((currentStack, index) =>
      if index < numberOfMatches then addCopies(currentStack, amountToAdd)
      else currentStack
    )

def calculateMatches(card: Scratchcard): Int =
  card.playedNumbers.intersect(card.winningNumbers).length

@tailrec
def calculateTotalCards(stacks: List[CardStack], totalCards: Int): Int =
  stacks match {
    case Nil => totalCards
    case currentStack :: tail =>
      val matches = calculateMatches(currentStack.card)
      val newTail = addBonusCards(tail, currentStack.copies, matches)

      calculateTotalCards(newTail, totalCards + currentStack.copies)
  }

val solutionTwo = calculateTotalCards(lines.map(getInitialStack), 0)
