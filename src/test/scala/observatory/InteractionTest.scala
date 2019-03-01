package observatory

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

import scala.collection.concurrent.TrieMap

trait InteractionTest extends FunSuite with Checkers {

  test("tileLocation") {
    val computed = Interaction.tileLocation(Tile(1, 1, 1))
    val expected = Location(0.0, 0.0)
    assert(computed == expected)
  }


  test("tileLocation 2 ") {
    val pos = 1
    val imageWidth = 2
    val x = 0
//    val computed = (pos % imageWidth).toDouble / imageWidth + x
    val computed = pos * imageWidth + x

    // val computed = Interaction.tileLocation(Tile(1, 1, 1))
    // val expected = Location(0.0, 0.0)

    val expected = 0.5
    assert(computed == expected)
  }



}
