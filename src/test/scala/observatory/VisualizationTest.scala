package observatory


import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

trait VisualizationTest extends FunSuite with Checkers {

  val testTemp = Seq(
    (Location(32.95, 65.56), 15.00), (Location(58.43, 5.80), 25.00),
    (Location(41.65, 12.43), 10.00)
  )

  test("predictTemperature") {
    val computed = Visualization.predictTemperature(testTemp, Location(58.43, 5.8))
    val expected = 25.00
    assert(computed == expected)
  }

  val testPoints = Seq(
    (60.0, Color(255, 255, 255)), (32.0, Color(255, 0, 0)), (12.0, Color(255, 255, 0)),
    (0.0, Color(0, 255, 255)), (-15.0, Color(0, 0, 255)), (-27.0, Color(255, 0, 255)))

  test("interpolateColor 1") {
    val cols = List[(Temperature, Color)]((1, Color(2, 2, 2)))
    val value = 2

    var computed = Visualization.interpolateColor(cols, value)
    val expected = Color(2, 2, 2)
    assert(computed == expected)

  }

  test("interpolateColor 2") {
    val cols = List[(Temperature, Color)]((1, Color(2, 2, 2)), (-1, Color(4, 4, 4)))

    var value = 1
    var computed = Visualization.interpolateColor(cols, value)
    var expected = Color(2, 2, 2)
    assert(computed == expected)

    value = 0
    computed = Visualization.interpolateColor(cols, value)
    expected = Color(3, 3, 3)
    assert(computed == expected)
  }


  test("interpolateColor 3") {
    val cols = List((0.0,Color(255,0,0)), (2.147483647E9,Color(0,0,255)))
    val value = 1.0737418235E9

    var computed = Visualization.interpolateColor(cols, value)
    val expected = Color(128,0,128)
    assert(computed == expected)

  }
  
  test("visualize") {
    val computedImage = Visualization.visualize((Location(0.0, 0.0), 0.0) :: Nil, (0.0, Color(0, 0, 0)) :: Nil)

    assert(computedImage.width === 360)
    assert(computedImage.height === 180)
  }
}