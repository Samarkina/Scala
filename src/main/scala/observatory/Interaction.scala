package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  val WIDTH = 256
  val HEIGHT = 256

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = {
    val n = 1 << tile.zoom
    val lon_deg = tile.x / n * 360.0 - 180.0
    val lat_rad = Math.atan(Math.sinh(Math.PI * (1.0 - 2.0 * tile.y / n)))
    val lat_deg = lat_rad * 180.0 / Math.PI

    Location(lat_deg, lon_deg)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors       Color scale
    * @param tile         Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {

    val pixels = (0 until HEIGHT * WIDTH)
      .par.map(pos => {
      val x = ((pos % HEIGHT).toDouble / HEIGHT + tile.x).toInt
      val y = ((pos / WIDTH).toDouble / WIDTH + tile.y).toInt

      val c = Visualization.interpolateColor(
        colors,
        Visualization.predictTemperature(
          temperatures,
          tileLocation(Tile(x, y, tile.zoom))
        )
      )

      Pixel(c.red, c.green, c.blue, 127)
    }).toArray



      Image.apply(WIDTH, HEIGHT, pixels)

  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    *
    * @param yearlyData    Sequence of (year, data), where `data` is some data associated with
    *                      `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
                           yearlyData: Iterable[(Year, Data)],
                           generateImage: (Year, Tile, Data) => Unit
                         ): Unit = {
    val tiles = for {
      zoom <- 0 until 4
      x <- 0 until (1 << zoom)
      y <- 0 until (1 << zoom)
      yearData <- yearlyData
    } yield generateImage(yearData._1, Tile(x, y, zoom), yearData._2)
  }

}
