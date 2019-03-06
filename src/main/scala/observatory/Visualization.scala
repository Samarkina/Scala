package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  val EARTH_RADIUS = 6371.0
  val RADIANS = math.Pi / 180.0
  val p = 2.0 //greater or equal to 2
  val WIDTH = 360
  val HEIGHT = 180

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {

    def distance(thatLocation: Location): Double = {
      val deltaLambda = math.abs(location.lon - thatLocation.lon)
      EARTH_RADIUS * math.acos(
        math.sin(location.lat * RADIANS) * math.sin(thatLocation.lat * RADIANS) +
        math.cos(location.lat * RADIANS) * math.cos(thatLocation.lat * RADIANS) * math.cos(deltaLambda * RADIANS)
      )
    }
    def idw(distanceTemp: Iterable[(Double, Double)], p: Double): Double = {
      val wTemp = distanceTemp.map(pair => (1.0 / math.pow(pair._1, p), pair._2 / math.pow(pair._1, p)))
      val u = wTemp.foldLeft((0.0,0.0))((wi, ui) => (wi._1 + ui._1, wi._2 + ui._2))
      u._2/u._1
      }

    val distanceTemp = temperatures.map(locTemp => (distance(locTemp._1), locTemp._2))

    // less than 1 km
    distanceTemp.find(_._1 < 1.0) match {
      case Some((_, temp)) => temp
      case None => idw(distanceTemp, p)
    }
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    def getColor(value: Temperature): Color = {
      val sortedPartition = points.toList.sortWith((x1, x2) => x1._1 < x2._1)
        .partition(x => x._1 < value)

      if (sortedPartition._1.isEmpty) sortedPartition._2.head._2
      else if (sortedPartition._2.isEmpty) sortedPartition._1.last._2
      else {
        val (lowTempCol, upTempCol) = (sortedPartition._1.last, sortedPartition._2.head)

        def linearInter(y0: Temperature, y1: Temperature): Int = {
          math.round(y0 + (value - lowTempCol._1) * (y1 - y0) / (upTempCol._1 - lowTempCol._1)).toInt
        }

        Color(
          linearInter(lowTempCol._2.red, upTempCol._2.red),
          linearInter(lowTempCol._2.green, upTempCol._2.green),
          linearInter(lowTempCol._2.blue, upTempCol._2.blue)
        )
      }
    }

    points.find(_._1 == value) match{
      case Some((_, color)) => color
      case None => getColor(value)
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {

    val locations = (for {
      lat <- 90 until -90 by -1
      lon <- -180 until 180 by 1
    } yield Location(lat, lon)).par

    val locationTemperatures = locations.map(l => predictTemperature(temperatures, l))
    val locationColors = locationTemperatures.map(t => interpolateColor(colors, t))
    val pixels = locationColors.map(c => Pixel(c.red, c.green, c.blue, 255)).toArray

    Image.apply(WIDTH, HEIGHT, pixels)

  }

}

