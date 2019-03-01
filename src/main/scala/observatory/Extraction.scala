package observatory

import java.time.LocalDate

import org.apache.spark.sql.SparkSession
import java.time.LocalDate
import java.nio.file.Paths

import org.apache.spark.sql._
import org.apache.spark.sql.types._

/**
  * 1st milestone: data extraction
  */

case class LocalTempCel(Month: Integer, Day: Integer, Latitude: Double, Longitude: Double, TempCel: Double)

object Extraction {

  import org.apache.spark.sql.SparkSession
  import org.apache.spark.sql.functions.rand
  import org.apache.log4j.{Level, Logger}
  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)


  val spark: SparkSession =
    SparkSession
      .builder()
      .appName("Observatory")
      .config("spark.master", "local")
      .getOrCreate()

  import spark.implicits._

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    val stations = readStations(stationsFile)
    val temperatures = readTemperatures(temperaturesFile)

    val merged = temperatures.join(stations)
      .where(temperatures.col("STN") === stations.col("STN") && temperatures.col("WBAN") === stations.col("WBAN"))
      .select(temperatures.col("Month"), temperatures.col("Day"), stations.col("Latitude"), stations.col("Longitude"), temperatures.col("TempCel"))
      .as[LocalTempCel]
      .collect()

    merged.par.map(row => (LocalDate.of(year, row.Month, row.Day), Location(row.Latitude, row.Longitude), row.TempCel)).seq


  }

  def readStations(stationsFile: String): DataFrame = {
    val dataSchema = StructType(Array(
      StructField("STN", StringType, true),
      StructField("WBAN", StringType, true),
      StructField("Latitude", DoubleType, true),
      StructField("Longitude", DoubleType, true)
    ))

    val rawData = spark.read.schema(dataSchema).csv(fsPath(stationsFile))

    rawData.filter(rawData.col("Latitude").isNotNull && rawData.col("Longitude").isNotNull)
      .na.fill("-")
  }

  def fsPath(resource: String): String = {
    Paths.get(getClass.getResource(resource).toURI).toString
  }

  def readTemperatures(temperaturesFile: String): DataFrame = {
    val dataSchema = StructType(Array(
      StructField("STN", StringType, true),
      StructField("WBAN", StringType, true),
      StructField("Month", IntegerType, true),
      StructField("Day", IntegerType, true),
      StructField("TempFahr", DoubleType, true)
    ))

    val rawData = spark.read.schema(dataSchema).csv(fsPath(temperaturesFile))

    rawData.filter(rawData.col("Month").isNotNull && rawData.col("Day").isNotNull && rawData.col("TempFahr").isNotNull)
      .na.fill("-")
      .withColumn("TempCel", (rawData.col("TempFahr") - 32.0) * (5.0 / 9.0))
      .drop("TempFahr")
  }


    /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    records.par.groupBy(_._2)
      .mapValues(
        rec => {
          rec.foldLeft(0.0)((averageSum, r) => averageSum + (r._3/rec.size))
        })
      .seq
  }

}
