@main def day5: Unit =
  // val inputLines = io.Source.fromFile("src/main/scala/day5/example.txt").getLines.toList
  val inputLines = io.Source.fromFile("src/main/scala/day5/input.txt").getLines.toList
  // day5Puzzle1(inputLines)
  day5Puzzle2(inputLines)

def day5Puzzle1(inputLines: List[String]): Unit = {
  val (seeds, mappings) = parseInput2(inputLines)
  val locations = calculateLocations(seeds.map(_.num), mappings)
  println(seeds)
  println(locations)
  println(locations.min)
}

def day5Puzzle2(inputLines: List[String]): Unit = {
  val (inpSeeds, mappings) = parseInput2(inputLines)
  val locations = inpSeeds.sliding(2, 2).map(s => {
    val (start, length) = (s(0).num, s(1).num)
    val seeds = start to start + length
    val licMin = calculateLocations(seeds.toList, mappings).min
    println(licMin)
    licMin
  }).toList
  println(locations.min)
  // val locations = calculateLocations(seeds, mappings)
  // println(locations.min)
}

def calculateLocations(seeds: List[BigInt], mappings: List[List[SourceToDestination]]): List[BigInt] = {
  val locations = seeds.map(seed => {
    val dst = mappings.foldLeft(seed)((nextDest, stdMaps) => {
      val dst = stdMaps.map(st => {
        if (nextDest >= st.source && nextDest < (st.source + st.length)) {
          Some(nextDest + st.destination - st.source)
        } else {
          None
        }
      }).filter(_.isDefined).headOption.flatten.getOrElse(nextDest)
      dst
    })
    dst
  })
  locations
}

case class Seed(num: BigInt)
case class SourceToDestination(source: BigInt, destination: BigInt, length: BigInt)
case class Mappings(
  seedToSoil: SourceToDestination,
  soilToFertilizer: SourceToDestination,
  fertilizerToWater: SourceToDestination,
  waterToLight: SourceToDestination,
  lightToTemperature: SourceToDestination,
  temperatureToHumidity: SourceToDestination,
  humidityToLocation: SourceToDestination,
)

case class ParseState(seeds: List[Seed] = List(), currentStage: String = "", mappings: List[List[SourceToDestination]] = List(), currentMapping: List[SourceToDestination] = List())

def parseInput2(inputLines: List[String]) = {
  val initialState = ParseState()

  val finalState = inputLines.foldLeft(initialState) { (state, line) =>
    line match {
      case l if l.contains("seeds:") =>
        val seeds = l.split(": ")(1).split(" ").map(_.trim).map(BigInt.apply).toList
        state.copy(seeds = seeds.map(Seed.apply))
      case l if l.contains("map:") =>
        state.copy(currentStage = l.split(":")(0).trim)
      case l if l.matches("\\d.*") => {
        val numbers = l.split("\\s+").map(BigInt.apply)
        val std = SourceToDestination(source = numbers(1), destination = numbers(0), length = numbers(2))
        state.copy(currentMapping = state.currentMapping :+ std)
      }
      case l if l.trim().isEmpty() =>
        state.copy(mappings = state.mappings.appended(state.currentMapping), currentMapping = List())
      case _ => state
    }
  }

  // Let's just use the list, I don't think we need the map
  (finalState.seeds, finalState.mappings.filter(_.nonEmpty))

  // finalState.mappings + (finalState.currentStage -> finalState.currentMapping)
}