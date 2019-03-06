package observatory

/**
  * 4th milestone: value-added information
  */
object Manipulation {

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Temperature)]): GridLocation => Temperature = {
    val allGrid = {
      for {
        lat <- -89 to 90
        lon <- -180 to 179
      } yield (GridLocation(lat, lon), Visualization.predictTemperature(temperatures, Location(lat, lon)))
    }.toMap

    gl => allGrid(gl)
  }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Temperature)]]): GridLocation => Temperature = {
    val grids = temperaturess.map(
      gridYear => makeGrid(gridYear)
    )
    val averTemp = {
      for {
        lat <- -89 to 90
        lon <- -180 to 179
      } yield (GridLocation(lat, lon), grids.map(grid => grid(GridLocation(lat, lon))).sum / grids.size)
    }.toMap

    gl => averTemp(gl)
  }

  /**
    * @param temperatures Known temperatures
    * @param normals A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Temperature)], normals: GridLocation => Temperature): GridLocation  => Temperature = {
    val normalTemp = {
      for {
        lat <- -89 to 90
        lon <- -180 to 179
      } yield (GridLocation(lat, lon), Visualization.predictTemperature(temperatures, Location(lat, lon)) - normals(GridLocation(lat, lon)))
    }.toMap

    gl => normalTemp(gl)
  }
}

