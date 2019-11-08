package quuux.semtric.io.test

import org.scalatest._
import quuux.semtric._
import quuux.semtric.Relation._
import quuux.semtric.Item._

class NetworkReaderWriterTest extends FlatSpec with Matchers {

  /** Constructs a network with all item types. */
  def diverseNetwork = {
    val iNone = INone
    val iString = IString("Genki desu ka.")
    val iStringUnicode = IString("元気ですか。")
    val iStringEmpty = IString("")
    val iStringWhite = IString(" \t")
    val iStringNewLine = IString("a\nmulti\nline\n\ntext")
    val iBoolTrue = IBool(true)
    val iBoolFalse = IBool(false)
    val iInt = IInt(2323908)
    val iLong = ILong(10000000l)
    val iIntZero = IInt(0)
    val iIntNegative = IInt(-74)
    val iReal = IReal(3.1415)
    val iRealZero = IReal(0)
    val iRealExp = IReal(-2e20)
    val iDate = IDateTime("1966-09-20 10:34")
    val iURI = IURI("audio/mp3", "file:///mysong.mp3")

    val items: Vector[AnyItem] = Vector(iNone, iString, iStringUnicode, iStringEmpty,
      iStringWhite, iStringNewLine, iBoolTrue, iBoolFalse, iInt, iIntZero, iIntNegative,
      iLong, iReal, iRealZero, iRealExp, iDate, iURI)

    val relations: Vector[AnyRelation] =
      Vector(Relation(iNone, iString), Relation(iString, iInt), Relation(iString, iLong),
        Relation(iReal, iDate), Relation(iString, iURI),
        Relation(iString, iBoolTrue), Relation(iString, iBoolFalse),
        Relation(iReal, iIntZero), Relation(iStringWhite, iStringEmpty))

    Network(items, relations)
  }

  /** Constructs random networks with random content strings.*/
  def randomNetwork(printable: Boolean) = {
    val rnd = new scala.util.Random
    def randomText: String = {
      val text = rnd.nextString(500)
      if (printable) text.replaceAll("\\p{Cntrl}", "") else text
    }
    val (n, m) = (1 + rnd.nextInt(50), rnd.nextInt(10))
    val items1 = (0 to n).map(_ => IString(randomText))
    val items2 = (0 to m).map(_ => IString(randomText))
    val relations = for (i <- 0 to n; j <- 0 to m) yield Relation(items1(i), items2(j))
    val itemSet = (items1 ++ items2).toSet[AnyItem]
    Network(itemSet.toVector, relations.toVector)
  }

  /** Prints detailed comparison in case of non-matching networks. */
  def assert(given: Network, expected: Network) {
    for (item <- expected.items) { // compare item sets.
      assert(given.items.toSet.contains(item), "Missing item: " + item.description)
      val givenItem = given.items.find(_.uuid == item.uuid).get
      givenItem.value should be(item.value)
    }
    val givenRelations = given.relations.toSet
    for (relation <- expected.relations) // compare relation sets.
      assert(givenRelations.contains(relation), "Missing relation: " + relation)
    given should be(expected) // compare entire networks.    
  }
    
}