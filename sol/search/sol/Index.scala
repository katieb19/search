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
  private var WordstoPage = new HashMap[String, HashMap[Int, Double]] //string word -->[int (id) -> how often word appears]
  private val idToLinks = new HashMap[Int, mutable.HashSet[String]] //id to linked pages
  private val idToTitle = new HashMap[Int, String]
  private val idToRank = new HashMap[Int, Double]

  //Local Helper HashTables
  private val innerMaxFreq = new HashMap[Int, Double] //id -> max freq

  //Converts xml file to nodes
  val mainNode: Node = xml.XML.loadFile(inputFile)

  //to access all pages
  val pageSeg: NodeSeq = mainNode \ "page"

  //Total number of pages
  val n = idToLinks.size

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

      //Populate WordsToPage
      val idSeq: NodeSeq = page \ "text"

      //Converting text tags to all strings
      val textIdSeq: String = idSeq.text

      //Tokenizing
      val regex = new Regex("""\[\[[^\[]+?\]\]|[^\W_]+'[^\W_]+|[^\W_]+""")

      // Call findAllMatchIn to get an iterator of Matches
      val matchesIterator = regex.findAllMatchIn(textIdSeq)

      // Convert the Iterator to a List and extract the matched substrings
      val matchesList = matchesIterator.toList.map { aMatch =>
        aMatch.matched
      }

      //for loop
      for (m <- matchesList) {
        // if m is a link (regex)
        if (m.matches("\\[\\[[^\\[]+?\\]\\]")) {
          //case that link doesnt have category or |
          if (!m.contains("|") && !m.contains("Category:")) {
            val newSet = new String
            //take out square brackets
            val regex2 = new Regex("([A-Z])\\w+")
            val matchesIterator2 = regex2.findAllMatchIn(m)

            // Convert the Iterator to a List and extract the matched substrings
            val matchesList2 =
              matchesIterator2.toList.map { aMatch =>
                aMatch.matched
              }
            for (m <- matchesList2) {
              addFunWordtoPage(trimId, m, WordstoPage)
              newSet + m
            }
            idToLinks(trimId).add(newSet)
          }
          //case that link has Presidents|Washington
          else if (m.contains("|")) {
            val splitWord = m.split("[|]")
            for (wd <- splitWord) {
              //if word before the |
              if (splitWord(0).contains(wd)) {
                idToLinks(trimId).add(wd)
              } else {
                addFunWordtoPage(trimId, m, WordstoPage)
              }
            }
          }

          //else if category [Category: Computer Science] --> Category: Computer Science
          // category, computer, science --> added to word list
          else {
            //regex to take out square brackets and isolate word
            val newSet = new String
            val regex3 = new Regex("([A-Z])\\w+")
            val matchesIterator2 = regex3.findAllMatchIn(m)
            //populate idtolink and wordstopage hashmap
            val matchesList2 =
              matchesIterator2.toList.map { aMatch =>
                aMatch.matched
              }
            for (m <- matchesList2) {
              addFunWordtoPage(trimId, m, WordstoPage)
              newSet + m
            }
            idToLinks(trimId).add(newSet)
          }
        }

        // else check if word isnt a stop word
        else if (!isStopWord(m)) {
          //Populate word to freq table
          addFunWordtoPage(trimId, m, WordstoPage)
        }
      }
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
                        wordPageHelper: mutable.HashMap[String, mutable.HashMap[Int, Double]]
                      ): Unit = {

    val stemWord = stem(word)
    //adds a word to a hashmap
    if (!wordPageHelper.contains(stemWord)) {
      //create new entry in hashmap wordpagehelper
      val addHash = new mutable.HashMap[Int, Double]
      addHash(id) = 1
      wordPageHelper += (stemWord -> addHash)

    } else {
      //add to the value of the inner hashmap by 1 based on the corresponding
      //id
      for ((wrd, innerMap) <- wordPageHelper) {
        if (stemWord.equals(wrd)) {
          for ((innerId, value) <- innerMap) {
            if (id == innerId) {
              innerMap(id) = value + 1
            }
          }
        }
      }
    }
  }

  /**
   * Helper to get title from id
   *
   * @param id : given document id
   * @return title corresponding to that id
   */
  def getTitle(id: Int): String = {
    var result = ""
    for ((key, title) <- idToTitle) {
      if (key == id) {
        result = title
      }
    }
    result
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

    //If page doesn't link to anything
    if (unique == 0) {
      for ((id, array) <- idToLinks) {
        if (id != jID) {
          array.add(getTitle(jID))
        }
      }
    }

    //get title of j
    val titleJ = idToTitle.get(jID)

    //Epsilon
    val epsilon = 0.15

    //If k links to j
    if (idToLinks.get(kID).contains(titleJ)) {
      (epsilon / n.toDouble) + ((1 - epsilon) / unique)
    } else {
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

  def distance(previous: mutable.HashMap[Int, Double],
               current: mutable.HashMap[Int, Double]): Double = {
    var sumDifferences = 0.0
    //    for (k <- 0 until previous.size) {
    //      sumDifferences += scala.math.pow(curr - element, 2)
    //    }
    for (element <- previous.values) {
      for (curr <- current.values) {
        sumDifferences += scala.math.pow(curr - element, 2)
      }
    }
    scala.math.sqrt(sumDifferences)
  }

  /**
   * Calculates the authority (pageRank) for each document, populating idToRank
   */
  def pageRank(): Unit = {

    //Size idToTitle
    val n = idToTitle.size

    // hashmap --> key is the page; val is a hashtable (key: page, value: weight))
    var previousR = new mutable.HashMap[Int, Double]

    //array of n zeros //Hashmap (key: id, value: array)
    val currentR = new mutable.HashMap[Int, Double]
    for ((id, _) <- idToTitle) {
      previousR.put(id, 0)
      currentR.put(id, 1 / n.toDouble)
    }

    while (distance(previousR, currentR) > 0.0001) {
      previousR = currentR
      //      for ((currID, _) <- currentR) {
      //        currentR(currID) = 0.0
      //        for ((prevID, _) <- previousR) {
      //          currentR(currID) = currentR(currID) + (weight(currID, prevID) * previousR(prevID))
      //        }
      //      }

      for (j <- 0 until n) {
        currentR(j) = 0.0
        for (k <- 0 until n) {
          currentR(j) = currentR(j) + (weight(j, k) * previousR(k))
        }
      }
    }

    //Populate idToRank
    for ((id, value) <- currentR) {
      idToRank.put(id, value)
    }
  }

  /**
   * Calculates the max Frequency of each word in a document and populates the inner Max Freq helper
   * The Inner Max Freq helper is the value of WordsToPage Hashmap
   */
  //Populates idsToMaxFreqs
  def innerMaxFreq2(): Unit = {

    //new hashmap w key: id, val: map(key: word, val: freq)
    val innerMap = new HashMap[String, Double]
    val idToWordFreq = new HashMap[Integer, HashMap[String, Double]]
    //populate inner hash map
    for ((id, _) <- idToTitle) {
      for ((wd, timesMap) <- WordstoPage) {
        for ((id2, totalTimes) <- timesMap) {
          if (id2 == id) {
            innerMap(wd) = totalTimes
          }
        }
      }
      idToWordFreq(id) = innerMap
    }

    // get max freq and populate innerMax Freq
    for ((id, inside) <- idToWordFreq) {
      //Track Variables
      var currMax = 0.0
      for ((wd, freq) <- inside) {
        if (freq > currMax) {
          currMax = freq
        }
      }
      innerMaxFreq(id) = currMax
    }
    //    //Calculating Max Frequency
    //    for ((id, _) <- idToTitle) {
    //      for ((_, timesMap) <- WordstoPage) {
    //        for ((id2, totalTimes) <- timesMap) {
    //          if (id2 == id) {
    //            if (totalTimes > currMax) {
    //              currMax = totalTimes
    //            }
    //          }
    //        }
    //      }
    //      innerMaxFreq.put(id, currMax)
    //    }
  }

  //Calling methods
  looping()
  innerMaxFreq2()
  pageRank()
}

object Index {
  def main(args: Array[String]) {
    val Index1 = new Index(args(0))
    println(Index1.idToRank)
    Index1.pageRank()
    Index1.looping()
    Index1.innerMaxFreq2()
    println(Index1.WordstoPage)
    println(Index1.innerMaxFreq)

    //Print calls
    printDocumentFile(args(2), Index1.innerMaxFreq, Index1.idToRank)
    printTitleFile(args(1), Index1.idToTitle)
    printWordsFile(args(3), Index1.WordstoPage)
  }
}