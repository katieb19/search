package search.sol

import search.src.FileIO.{printDocumentFile, printTitleFile, printWordsFile}
import search.src.PorterStemmer.stem
import search.src.StopWords.isStopWord

import scala.collection.mutable
import scala.collection.mutable.HashMap
import scala.util.matching.Regex
import scala.xml.{Node, NodeSeq}

/**
 * Provides an XML indexer, produces files for a querier
 *
 * @param inputFile - the filename of the XML wiki to be indexed
 */
class Index(val inputFile: String) {

  //HashTables to be read in Query
  private val WordstoPage = new HashMap[String, HashMap[Int, Double]]
  private val idToLinks = new HashMap[Int, mutable.HashSet[String]]
  private val idToTitle = new HashMap[Int, String]
  private val idToRank = new HashMap[Int, Double]
  private val innerMaxFreq = new HashMap[Int, Double] //id -> max freq

  //Converts xml file to nodes
  val mainNode: Node = xml.XML.loadFile(inputFile)

  //to access all pages
  var pageSeg: NodeSeq = mainNode \ "page"

  //Total number of pages
  val n: Int = idToLinks.size


  def looping(): Unit = {
    for (page <- pageSeg) {

      //Getting title and ID from page
      val id: NodeSeq = page \ "id"
      val trimId = id.text.trim.toInt
      val title: NodeSeq = page \ "title"
      val trimTitle = title.text.trim
      //TRIM TITLE

      //Populate idToTitle
      idToTitle(trimId) = trimTitle

      val idSeq: NodeSeq = page \ "text"

      //Converting text tags to all strings
      val textIdSeq: String = idSeq.text

      //Tokenizing
      val regex = new Regex("""\[\[[^\[]+?\]\]|[^\W_]+'[^\W_]+|[^\W_]+""")

      // Call findAllMatchIn to get an iterator of Matches
      val matchesIterator = regex.findAllMatchIn(textIdSeq)

      // Convert the Iterator to a List and extract the matched substrings
      var matchesList = matchesIterator.toList.map { aMatch =>
        aMatch.matched
      }


      //for loop
      for (m <- matchesList) {
        // if m is a link (regex)
        if (m.matches("\\[\\[[^\\[]+?\\]\\]")) {
          //case that link doesnt have category or |
          if (!m.contains("|") && !m.contains("Category:")) {
            plainLink(m, trimId)
          }
          //case that link has Presidents|Washington
          else if (m.contains("|")) {
            dashLink(m, trimId)
          }
          //else if category [Category: Computer Science] -->
          // Category: Computer Science
          // category, computer, science --> added to word list
          else {
            categoryLink(m, trimId)
          }
        }

        // else check if word isnt a stop word
        else if (!isStopWord(m)) {
          //Populate word to freq table
          addFunWordtoPage(trimId, m.toLowerCase(), WordstoPage)
        }
        matchesList = matchesList.tail
      }
      pageSeg = pageSeg.tail
    }
  }


  def dashLink(m: String, id: Int): Unit = {
    //take out square brackets
    val subString = m.substring(2, m.length - 2)
    val splitWord = subString.split("[|]")
    for (wd <- splitWord) {
      //if word before the |
      if (wd == splitWord(0)) {
        if (idToLinks.contains(id)) {
          idToLinks(id) + wd
        }
        else {
          val set2 = new mutable.HashSet[String]()
          idToLinks(id) = set2 + wd
        }
      }
      else if (wd != splitWord(0)) {
        val regex3 = new Regex("([A-Z0-9+])\\w+")
        val matchesIterator3 = regex3.findAllMatchIn(wd)
        var matchesList3 =
          matchesIterator3.toList.map { aMatch =>
            aMatch.matched
          }
        for (wd2 <- matchesList3) {
          addFunWordtoPage(id, wd2.toLowerCase(), WordstoPage)
          matchesList3 = matchesList3.tail
        }
      }
    }
  }

  def plainLink(m: String, id: Int): Unit = {
    //empty string to populate if there is a link
    var newSet = new String
    //take out square brackets
    val regex3 = new Regex("([A-Z0-9+])\\w+")
    val matchesIterator2 = regex3.findAllMatchIn(m)
    var matchesList2 =
      matchesIterator2.toList.map { aMatch =>
        aMatch.matched
      }
    for (m2 <- matchesList2) {
      addFunWordtoPage(id, m2.toLowerCase(), WordstoPage)
      newSet += m2
      matchesList2 = matchesList2.tail
    }
    if (idToLinks.contains(id)) {
      idToLinks(id) += newSet
    }
    else {
      idToLinks(id) = new mutable.HashSet[String]() + newSet
    }
  }

  def categoryLink(m: String, id: Int): Unit = {
    val subString = m.substring(2, m.length - 2)
    //populate idtolink and wordstopage hashmap
    val regex3 = new Regex("([A-Z])\\w+")
    val matchesIterator4 = regex3.findAllMatchIn(m)
    var matchesList4 =
      matchesIterator4.toList.map { aMatch =>
        aMatch.matched
      }
    for (m <- matchesList4) {
      addFunWordtoPage(id, m.toLowerCase(), WordstoPage)
      matchesList4 = matchesList4.tail
    }
    if (idToLinks.contains(id)) {
      idToLinks(id) + subString
    }
    else {
      idToLinks(id) = new mutable.HashSet[String]() + subString
    }
  }

  /**
   * Adds word to WordToPages HashMap
   *
   * @param id             - document ID
   * @param word           - word (String)
   * @param wordPageHelper - WordsToPage HashTable
   */
  def addFunWordtoPage(
                        id: Int,
                        word: String,
                        wordPageHelper:
                        mutable.HashMap[String, mutable.HashMap[Int, Double]]
                      ): Unit = {
    val stemWord = stem(word).toLowerCase()
    //adds a word to a hashmap
    if (!wordPageHelper.contains(stemWord)) {
      //create new entry in hashmap wordpagehelper
      val addHash = new mutable.HashMap[Int, Double]
      addHash(id) = 1
      wordPageHelper += (stemWord -> addHash)

    }
    else {
      //add to the value of the inner hashmap by 1 based on the corresponding
      //id
      for ((wrd, innerMap) <- wordPageHelper) {
        if (stemWord.equals(wrd)) {
          for ((innerId, value) <- innerMap) {
            if (id == innerId) {
              innerMap(id) = value + 1
            }
            else {
              innerMap(id) = 1
            }
          }
        }
      }
    }
  }


  /**
   * Calculate the weight of page k on page j
   *
   * @param kID : id of page k
   * @param jID : id of page j
   * @return a double representing the square root of the sum of differences
   */
  def weight(kID: Int, jID: Int): Double = {

    //Total number of unique pages that k links to
    val unique = idToLinks.get(kID).size

    //Size idToTitle
    val n = idToTitle.size

    //get title of j
    val titleJ = idToTitle.get(jID).toString

    //Epsilon
    val epsilon = 0.15

    //If page doesn't link to anything
    if (unique == 0) {
      (epsilon / n.toDouble) + ((1 - epsilon) / (n - 1))
    }

    //If k links to j
    else if (idToLinks(kID).contains(titleJ) &&
      !idToLinks(kID).contains(idToTitle.get(kID).toString)) {
      (epsilon / n.toDouble) + ((1 - epsilon) / unique)

    }
    //if page links to itself
    else if (idToLinks(kID).contains(idToTitle.get(kID).toString)) {
      epsilon / n.toDouble
    }
    else if (idToLinks(kID) == null) {
      0
    }
    else {
      epsilon / n.toDouble
    }
  }

  /**
   * Calculate the distance between r and r'
   *
   * @param previous : HashMap
   * @param current  : HashMap
   * @return a double representing the square root of the sum of differences
   */
  def distance(previous: Array[Double],
               current: Array[Double]): Double = {
    var sumDifferences = 0.0
    for (n <- previous.indices) {
      sumDifferences += scala.math.pow(current(n) - previous(n), 2)
    }
    scala.math.sqrt(sumDifferences)
  }

  /**
   * Calculates the authority (pageRank) for each document, populating idToRank
   */
  def pageRank(): Unit = {
    //Arbitrary number
    val n = 50

    //Number of pages
    val size = idToTitle.size

    // hashmap --> key is page; val is a hashtable (key: page, value: weight))
    var previousR = Array.fill[Double](size)(0.0)

    //array of n zeros //Hashmap (key: id, value: array)
    val currentR = Array.fill[Double](size)(1 / n.toDouble)

    //Calculates distance and weights
    while (distance(previousR, currentR) > 0.0001) {
      previousR = currentR

      var j = 0
      for ((id1, title) <- idToTitle) {
        if (idToTitle.contains(id1)) {
          currentR(j) = 0.0
        }
        var k = 0
        for ((id2, title) <- idToTitle) {
          if (idToTitle.contains(id2)) {
            currentR(j) += (weight(j, k) * previousR(k))
          }
          k = k + 1
        }
        idToRank(id1) = currentR(j)
        j = j + 1
      }
    }

  }

  /**
   * Calculates the max Frequency of each word in a document and populates
   * the inner Max Freq helper
   * The Inner Max Freq helper is the value of WordsToPage Hashmap with words
   * as keys and frequencies as values
   */
  def innerMaxFreq2(): Unit = {

    val idToWordFreq = new HashMap[Integer, HashMap[String, Double]]

    for ((id, _) <- idToTitle) {
      val innerMap = new HashMap[String, Double]
      for ((wd, timesMap) <- WordstoPage) {
        for ((id2, totalTimes) <- timesMap) {
          if (id2 == id) {
            innerMap(wd) = totalTimes
          }
        }
      }
      idToWordFreq(id) = innerMap
    }

    //Track Variables
    var currMax = 0.0

    // get max freq and populate innerMax Freq
    for ((id, inside) <- idToWordFreq) {
      for ((wd, freq) <- inside) {
        if (freq > currMax) {
          currMax = freq
        }
      }
      innerMaxFreq(id) = currMax
    }
  }

  //Calling methods
  looping()
  pageRank()
  innerMaxFreq2()
}

object Index {
  def main(args: Array[String]) {
    if (args.length != 4) {
      println("Not enough arguments")
    }
    else {
      val Index1 = new Index(args(0))
      printDocumentFile(args(2), Index1.innerMaxFreq, Index1.idToRank)
      printTitleFile(args(1), Index1.idToTitle)
      printWordsFile(args(3), Index1.WordstoPage)
    }
  }
}