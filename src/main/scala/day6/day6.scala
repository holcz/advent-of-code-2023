@main def day6: Unit =
  // val inputLines = io.Source.fromFile("src/main/scala/day6/example.txt").getLines.toList
  val inputLines = io.Source.fromFile("src/main/scala/day6/input2.txt").getLines.toList
  // day6Puzzle1(inputLines)
  day6Puzzle2(inputLines)

def day6Puzzle1(inputLines: List[String]): Unit = {
  val games = parseGames(inputLines)
  val res = games.map(g => {
    val time = g.time
    val oldRecord = g.distance
    val waysToWin = (0 to time).filter(t => {
      val dist = t * (time - t)
      dist > oldRecord
    })
    waysToWin.length
  }).product
  println(res)
}

def day6Puzzle2(inputLines: List[String]): Unit = {
  val games = parseGames2(inputLines)
  val res = games.map(g => {
    val time = g.time
    val oldRecord = g.distance
    val waysToWin = Iterator.iterate(0)(_ + 1).takeWhile(_ <= time).filter(t => {
      val dist = t * (time - t)
      dist > oldRecord
    })
    waysToWin.length
  }).product
  println(res)
}

case class BoatGame(time: Int, distance: Int)
case class BoatGame2(time: BigInt, distance: BigInt)

//Time:      7  15   30
//Distance:  9  40  200
def parseGames(inputLines: List[String]): List[BoatGame] = {
  val times = inputLines.head.split(":")(1).trim.split("\\s+").map(_.toInt)
  val distances = inputLines.last.split(":")(1).trim.split("\\s+").map(_.toInt)
  times.zip(distances).map((t, d) => BoatGame(t, d)).toList
}

def parseGames2(inputLines: List[String]): List[BoatGame2] = {
  val times = inputLines.head.split(":")(1).trim.split("\\s+").map(BigInt.apply)
  val distances = inputLines.last.split(":")(1).trim.split("\\s+").map(BigInt.apply)
  times.zip(distances).map((t, d) => BoatGame2(t, d)).toList
}