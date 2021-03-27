package search.sol

import tester.Tester

import scala.collection.mutable.HashMap

object TesterFile {

  private val emptyWordsToPages = new HashMap[String, HashMap[Int, Double]]
  private val singleWordsToPages = new HashMap[String, HashMap[Int, Double]]
  private val multipleWordsToPages = new HashMap[String, HashMap[Int, Double]]
  private val

  def setup() {
    singleWordsToPages.put("firstWord", HashMap.put(0, 1.0))

  }

  //Index Methods
  def testAddFunWordToPage(t: Tester): Unit = {
    //empty WordsToPages
    //word already exists
    //base case
  }

  def testGetTitle(t: Tester): Unit = {

  }

  def testWeight(t: Tester): Unit = {

  }

  def testDistance(t: Tester): Unit = {

  }

}
