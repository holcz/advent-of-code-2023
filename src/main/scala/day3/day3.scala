@main def day3: Unit =
  // val inputLines = io.Source.fromFile("src/main/scala/day3/example.txt").getLines.toList
  val inputLines = io.Source.fromFile("src/main/scala/day3/input.txt").getLines.toList
  // day3Puzzle1(inputLines)
  day3Puzzle2(inputLines)

def day3Puzzle1(inputLines: List[String]): Unit = {
  val symbolCoordinates = parseInput(inputLines)
  val numberCoordinates = parseToNumberCoordinates(symbolCoordinates)
  val engineParts = numberCoordinates.filter(nc => isAdjacentToSymbol(symbolCoordinates)(nc.coordinates))
  val result = engineParts.map(_.number).sum
  println(result)
}

def day3Puzzle2(inputLines: List[String]): Unit =
  val symbolCoordinates = parseInput(inputLines)
  val numberCoordinates = parseToNumberCoordinates(symbolCoordinates)
  val gears = symbolCoordinates.filter(_.symbol == '*')
  val result = gears.map(mapAdjacentToTwoPartNumber(numberCoordinates)).sum
  println(result)

case class Point(x: Int, y: Int)
case class NumberCoordinates(number: Int, coordinates: List[Point])
case class SymbolCoordinate(symbol: Char, coordinate: Point, isDigit: Boolean, isDot: Boolean)

def parseInput(inputLines: List[String]): List[SymbolCoordinate] =
  inputLines.zipWithIndex.flatMap { (line, y) =>
    line.zipWithIndex.map { (symbol, x) =>
      SymbolCoordinate(symbol, Point(x, y), symbol.isDigit, symbol == '.')
    }
  }

def parseToNumberCoordinates(symbolCoordinates: List[SymbolCoordinate]): List[NumberCoordinates] = {
  // Pretty imperative, I may refactor it later
  val stack = scala.collection.mutable.Stack[SymbolCoordinate]()
  val result = scala.collection.mutable.ListBuffer[NumberCoordinates]()
  symbolCoordinates.foreach { curr =>
    if (curr.isDigit) {
      stack.push(curr)
    } else if (!stack.isEmpty) {
      val coordinates = stack.map(_.coordinate).toList
      val number = stack.foldRight("")((curr, acc) => acc + curr.symbol.asDigit.toString()).toInt
      val numberCoordinates = NumberCoordinates(number, coordinates)
      stack.clear()
      result += numberCoordinates
    }
  }
  result.toList
}
  
def getNeighbours(point: Point): List[Point] =
  List(
    Point(point.x - 1, point.y - 1),
    Point(point.x, point.y - 1),
    Point(point.x + 1, point.y - 1),
    Point(point.x - 1, point.y),
    Point(point.x + 1, point.y),
    Point(point.x - 1, point.y + 1),
    Point(point.x, point.y + 1),
    Point(point.x + 1, point.y + 1)
  )

def isAdjacentToSymbol(all: List[SymbolCoordinate])(coordinates: List[Point]): Boolean =
  coordinates.exists { coordinate =>
      getNeighbours(coordinate).exists { neighbour =>
        // Pretty sloppy, regarding to performance
      all.exists { curr =>
        curr.coordinate == neighbour && !curr.isDigit && !curr.isDot
      }
    }
  }

def mapAdjacentToTwoPartNumber(partNumbers: List[NumberCoordinates])(gear: SymbolCoordinate): Int = {
  val neighbours = getNeighbours(gear.coordinate)
  val adjacentPartNumbers = partNumbers.filter(_.coordinates.exists(neighbours.contains))
  if (adjacentPartNumbers.length == 2) adjacentPartNumbers.map(_.number).product else 0
}