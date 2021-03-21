package search.sol

import search.src.PorterStemmer.stem
import search.src.StopWords.isStopWord

import scala.collection.mutable.HashMap
import scala.util.matching.Regex
import scala.xml.{Node, NodeSeq}

/**
 * Provides an XML indexer, produces files for a querier
 *
 * @param inputFile - the filename of the XML wiki to be indexed
 */
class Index(val inputFile: String) {
  // TODO : Implement!

  val mainNode: Node = xml.XML.loadFile(inputFile)

  //Hash tables
  private val idToTitle = new HashMap()[Int, String]
  private val idToWords = new HashMap()[String, HashMap[Int, Double]] //string word --> hashmap of int (id) to relevance (double
  private val idToLinks = new HashMap()[Int, Array[String]]


  def looping(): Unit = {

    //to access all pages
    val pageSeg: NodeSeq = mainNode \ "page"

    for (page <- pageSeg) {

      //Getting title and ID from page
      val id: NodeSeq = page \ "id"
      val title: NodeSeq = page \ "title"

      //Populate idToTitle
      idToTitle(id.text.toInt) = title.text

      //Populate idToWords
      val idSeq: NodeSeq = page \ "text"

      //Converting text tags to all strings
      val textIdSeq: String = idSeq.text

      //Tokenizing
      val regex = new Regex("""\[\[[^\[]+?\]\]|[^\W_]+'[^\W_]+|[^\W_]+""")

      // Call findAllMatchIn to get an iterator of Matches
      val matchesIterator = regex.findAllMatchIn(textIdSeq)

      // Convert the Iterator to a List and extract the matched substrings
      val matchesList = matchesIterator.toList.map { aMatch => aMatch.matched }

      //for loop
      for (m <- matchesList) {
        // if m is a link (regex)
        if (m.matches("\\[\\[[^\\[]+?\\]\\]") {
          //        then populate the id to link hashmap
          val newArray = new Array[String](1000)
          idToLinks(id.text.toInt) = newArray + m //how to add element to array
          //        then check if link is not category format
          //             if it isnt then populate word to freq table
          if (!m.contains("Category:")) {
            addFunWord(id.text.toInt, m, idToWords)
          }
        }
        // else check if word isnt a stop word
        else if (!isStopWord(m)) {
          //        then take the stem of said word
          val stemWord = stem(m)
          //        then populate word to freq table
          addFunWord(id.text.toInt, stemWord, idToWords)
        }
      }
    }
  }

  def addFunWord(id: Int, wd: String, hM: HashMap[String, HashMap[Int, Double]]): Unit = {
    //adds a word to a hashmap
    if (!hM.contains(wd)) {
      val addHash = new HashMap()[Int, Double]
      addHash(id) = 1
      hM += (wd -> addHash)
    }
    else {
      val currVal = hM(wd)(id)
      hM(wd)(id) = currVal + 1
    }
  }


}

object Index {
  def main(args: Array[String]) {
    // TODO : Implement!
    System.out.println("Not implemented yet!")
  }
}
