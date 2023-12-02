@main def day2: Unit =
  // val inputLines = io.Source.fromFile("src/main/scala/day2/example.txt").getLines.toList
  val inputLines = io.Source.fromFile("src/main/scala/day2/input.txt").getLines.toList
  // day2Puzzle1(inputLines)
  day2Puzzle2(inputLines)

def day2Puzzle1(inputLines: List[String]): Unit =
  val possibleGames = inputLines.map(parseGame).filter(game => {
    val redMax = game.colorCounts.filter(_._1 == Color.Red).map(_._2).max
    val greenMax = game.colorCounts.filter(_._1 == Color.Green).map(_._2).max
    val blueMax = game.colorCounts.filter(_._1 == Color.Blue).map(_._2).max
    val isPossible = redMax <= gameConfig(Color.Red) && greenMax <= gameConfig(Color.Green) && blueMax <= gameConfig(Color.Blue)
    isPossible
  })
  println(possibleGames.map(_.gameId).sum)

def day2Puzzle2(inputLines: List[String]): Unit =
  val powerOfGames = inputLines.map(parseGame).map(game => {
    val redMax = game.colorCounts.filter(_._1 == Color.Red).map(_._2).max
    val greenMax = game.colorCounts.filter(_._1 == Color.Green).map(_._2).max
    val blueMax = game.colorCounts.filter(_._1 == Color.Blue).map(_._2).max
    val power = redMax * greenMax * blueMax
    power
  })
  println(powerOfGames.sum)

enum Color:
  case Red, Green, Blue

case class Game(gameId: Int, colorCounts: Array[(Color, Int)])

val gameConfig: Map[Color, Int] = Map(
  Color.Red -> 12,
  Color.Green -> 13,
  Color.Blue -> 14
)

def parseGame(game: String) = {
  val splitGame = game.split(":")
  val gameId = splitGame(0).replace("Game ", "").trim.toInt
  val colorCounts = splitGame(1).split(";").flatMap { pair =>
    pair.split(",").map { colorCount =>
      val splitColorCount = colorCount.trim.split(" ")
      parseColor(splitColorCount(1)) -> splitColorCount(0).toInt
    }
  }
  Game(gameId, colorCounts)
}

def parseColor(color: String): Color = color match {
  case "red" => Color.Red
  case "green" => Color.Green
  case "blue" => Color.Blue
}
