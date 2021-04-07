package search.sol

import tester.Tester

import scala.collection.mutable.HashMap

object TesterFile {

  private val emptyWordsToPages = new HashMap[String, HashMap[Int, Double]]
  private val existWordsToPages = new HashMap[String, HashMap[Int, Double]]
  private val multipleWordsToPages = new HashMap[String, HashMap[Int, Double]]
  private val singleInner = new HashMap[Int, Double]
  private val multipleInner = new HashMap[Int, Double]

  def setupAddFunWordToPage() {
    singleInner.put(0, 2.0)
    existWordsToPages.put("firstword", singleInner)

    multipleInner.put(1, 3.0)
    multipleWordsToPages.put("firstWord", singleInner)
    multipleWordsToPages.put("secondWord", multipleInner)
  }

  def setupGetTitle(): Unit = {
    val idToTitle = new HashMap[Int, String]

    idToTitle.put(0, "dog")
    idToTitle.put(1, "hedgehog")
  }

  //Index Methods
  def testAddFunWordToPage(t: Tester): Unit = {
    setupAddFunWordToPage()
    val index = new
        Index("C:\\Users\\katie\\Documents\\CS18\\GitHub\\" +
          "search-cbaumga1-vagulia3-1\\sol\\search\\sol\\SmallerWiki.xml")

    //empty WordsToPages
    index.addFunWordtoPage(0, "lalala", emptyWordsToPages)
    t.checkExpect(emptyWordsToPages.size, 1)

    //word already exists
    index.addFunWordtoPage(0, "firstWord", existWordsToPages)
    t.checkExpect(existWordsToPages.size, 1)

    //base case
    index.addFunWordtoPage(2, "lalala", multipleWordsToPages)
    t.checkExpect(multipleWordsToPages.size, 3)
  }
}

object Main extends App {
  Tester.run(TesterFile)
}
