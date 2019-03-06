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

}
