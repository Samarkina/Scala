package observatory

import java.time.LocalDate

import org.scalatest.FunSuite

trait ExtractionTest extends FunSuite {

  val stationsFile = "/testStations.csv"
  val temperaturesFile = "/test2015.csv"

  val locateTemp = Extraction.locateTemperatures(2015, stationsFile, temperaturesFile)

  test("readStations") {
    val stations = Extraction.readStations(stationsFile)
    stations.show()
    stations.printSchema()
  }

  test("readTemperatures") {
    val temperatures = Extraction.readTemperatures(temperaturesFile)
    temperatures.show()
    temperatures.printSchema()
  }

  test("locateTemperatures") {
    val expected = Set(
      (LocalDate.of(2015, 8, 11), Location(37.35, -78.433), 27.3),
      (LocalDate.of(2015, 12, 6), Location(37.358, -78.438), 0.0),
      (LocalDate.of(2015, 1, 29), Location(37.358, -78.438), 2.000000000000001)
    )
    assert(locateTemp.toSet == expected)
  }

  test("locationYearlyAverageRecords") {
    val computed = Extraction.locationYearlyAverageRecords(locateTemp).toSet
    val expected = Set(
      (Location(37.35, -78.433), 27.3),
      (Location(37.358, -78.438), 1.0000000000000004)
    )
    assert(computed == expected)
  }

}