@main def day4: Unit =
  // val inputLines = io.Source.fromFile("src/main/scala/day4/example.txt").getLines.toList
  val inputLines = io.Source.fromFile("src/main/scala/day4/input.txt").getLines.toList
  // day4Puzzle1(inputLines)
  day4Puzzle2(inputLines)

def day4Puzzle1(inputLines: List[String]): Unit = {
  val cards = inputLines.map(parseCard)
  val res = cards.map( card => {
    val winning = findWinningNumbers(card).length
    if (winning > 0) scala.math.pow(2, winning-1).toInt else 0
  }).sum
  println(res)
}

def day4Puzzle2(inputLines: List[String]): Unit = {
  val cards: scala.collection.mutable.ListBuffer[Card2] =
    inputLines.map(parseCard2).to(scala.collection.mutable.ListBuffer)
  for (i <- 0 until cards.length) {
    val card = cards(i)
    val winning = findWinningNumbers(card).length
    val copies = card.copies
    for (j <- i+1 until i + 1 + winning) {
      val otherCard = cards(j)
      cards.update(j, otherCard.copy(copies = otherCard.copies + copies))
    }
  }
  val res = cards.map(_.copies).sum
  println(res)
}

case class Card(cardId: Int, winningNumbers: List[Int], myNumbers: List[Int])
case class Card2(cardId: Int, winningNumbers: List[Int], myNumbers: List[Int], copies: Int = 0)

def parseCard(card: String): Card = {
  val splitCard = card.split(":")
  val cardId = splitCard(0).replace("Card ", "").trim.toInt
  val numbers = splitCard(1).split("\\|").map(_.trim.split("\\s+").map(_.toInt).toList)
  Card(cardId, numbers(0), numbers(1))
}

def parseCard2(cardString: String): Card2 = {
  val card = parseCard(cardString)
  Card2(card.cardId, card.winningNumbers, card.myNumbers, 1)
}

def findWinningNumbers(card: Card | Card2): List[Int] =
  val (myNumbers, winningNumbers) = card match {
    case Card(_, winningNumbers, myNumbers) => (myNumbers, winningNumbers)
    case Card2(_, winningNumbers, myNumbers, _) => (myNumbers, winningNumbers)
  }
  myNumbers.filter(number => winningNumbers.contains(number))
