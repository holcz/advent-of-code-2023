def day1: Unit =
  // val inputLines = io.Source.fromFile("src/main/scala/day1/example.txt").getLines.toList
  val inputLines = io.Source.fromFile("src/main/scala/day1/day1.txt").getLines.toList
  // puzzle1(inputLines)
  puzzle2(inputLines)

def puzzle1(inputLines: List[String]): Unit =
  val result = inputLines.map(line => {
    val firstDigit = line.dropWhile(!_.isDigit).head
    val lastDigit = line.reverse.dropWhile(!_.isDigit).head
    s"$firstDigit$lastDigit".toInt
  }).sum
  println(result)

def puzzle2(inputLines: List[String]): Unit =
  val result = inputLines.map(line => {
    val reversed = line.reverse
    val firstIndex = line.zipWithIndex.dropWhile((char, index) => getLeadingDigit(line.drop(index)).isEmpty).head._2
    val lastIndex = reversed.zipWithIndex.dropWhile((char, index) => getLeadingDigitReversed(reversed.drop(index)).isEmpty).head._2
    val firstDigit = getLeadingDigit(line.drop(firstIndex))
    val lastDigit = getLeadingDigitReversed(reversed.drop(lastIndex))
    val res = s"${firstDigit.get}${lastDigit.get}".toInt
    res
  }).sum
  println(result)

val digitsSpelled = List(("one", 1), ("two", 2), ("three", 3), ("four", 4), ("five", 5), ("six", 6), ("seven", 7), ("eight", 8), ("nine", 9))
def getLeadingDigit(s: String): Option[Int] =
  if (s.head.isDigit) Some(s.head.asDigit) else digitsSpelled.find(curr => s.startsWith(curr._1)).map(_._2)

def getLeadingDigitReversed(s: String): Option[Int] =  
  if (s.head.isDigit) Some(s.head.asDigit) else digitsSpelled.find(curr => s.startsWith(curr._1.reverse)).map(_._2)
