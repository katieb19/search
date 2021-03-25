package search.sol

import search.src.FileIO.{printDocumentFile, printTitleFile, printWordsFile}
import search.src.PorterStemmer.stem
import search.src.StopWords.isStopWord

import scala.collection.mutable
import scala.collection.mutable.HashMap
import scala.math.log10
import scala.util.matching.Regex
import scala.xml.{Node, NodeSeq}

/**
  * Provides an XML indexer, produces files for a querier
  *
  * @param inputFile - the filename of the XML wiki to be indexed
  */
class Index(val inputFile: String) {
  //Hash tables
  private var WordstoPage = new HashMap[String, HashMap[Int, Double]] //string word -->[int (id) -> how often word appears]
  private val idToLinks = new HashMap[Int, mutable.HashSet[String]] //id to linked pages
  private val idToTitle = new HashMap[Int, String]
  private val idToRank = new HashMap[Int, Double]
  private val idToWords = new HashMap[Int, HashMap[String, Double]] //id to [Words, # times it appears]
  //helper id -> max frequencies
  private val innerMaxFreq = new HashMap[Int, Double] //id -> max freq
  private val idToRelevance = new HashMap[String, HashMap[Int, Double]]

  val mainNode: Node = xml.XML.loadFile(inputFile)

  //Total number of pages
  val n = idToLinks.size

  def looping(): Unit = {

    //to access all pages
    val pageSeg: NodeSeq = mainNode \ "page"

    for (page <- pageSeg) {

      //Getting title and ID from page
      val id: NodeSeq = page \ "id"
      val title: NodeSeq = page \ "title"

      //Populate idToTitle
      idToTitle(id.text.toInt) = title.text
      for ((id, title) <- idToTitle) {
        idToWords.put(id, HashMap.empty[String, Double])
      }

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
              addFunidToWord(id.text.toInt, m, idToWords)
              addFunWordtoPage(id.text.toInt, m, WordstoPage)
              newSet + m
            }
            idToLinks(id.text.toInt).add(newSet)
          }
          //case that link has Presidents|Washington
          else if (m.contains("|")) {
            val splitWord = m.split("[|]")
            for (wd <- splitWord) {
              //if word before the |
              if (splitWord(0).contains(wd)) {
                idToLinks(id.text.toInt).add(wd)
              } else {
                addFunidToWord(id.text.toInt, wd, idToWords)
                addFunWordtoPage(id.text.toInt, m, WordstoPage)
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
              addFunidToWord(id.text.toInt, m, idToWords)
              addFunWordtoPage(id.text.toInt, m, WordstoPage)
              newSet + m
            }
            idToLinks(id.text.toInt).add(newSet)
          }
        }
        // else check if word isnt a stop word
        else if (!isStopWord(m)) {
          //        then populate word to freq table
          addFunidToWord(id.text.toInt, m, idToWords)
          addFunWordtoPage(id.text.toInt, m, WordstoPage)
        }
      }
    }
  }

  def addFunidToWord(
    id: Int,
    wd: String,
    hM: mutable.HashMap[Int, mutable.HashMap[String, Double]]
  ): Unit = {
    //adds a word to a hashmap
    if (!hM(id).contains(wd)) {
      //        then take the stem of said word
      val stemWord = stem(wd)
      val addHash = new mutable.HashMap[String, Double]
      addHash(wd) = 1.0
      hM(id) = addHash
    } else {
      //        then take the stem of said word
      val stemWord = stem(wd)
      val currVal = hM(id)(stemWord)
      hM(id)(stemWord) = currVal + 1.0
    }
  }

  /**
    * Provides an XML indexer, produces files for a querier
    *
    * @param id - document ID
    * @param wd - word (String)
    * @param hM - WordsToPage HashTable
    */
  def addFunWordtoPage(
    id: Int,
    wd: String,
    hM: mutable.HashMap[String, mutable.HashMap[Int, Double]]
  ): Unit = {
    //adds a word to a hashmap
    if (!hM.contains(wd)) {
      //        then take the stem of said word
      val stemWord = stem(wd)
      val addHash = new mutable.HashMap[Int, Double]
      addHash(id) = 1
      hM += (stemWord -> addHash)
    } else {
      //        then take the stem of said word
      val stemWord = stem(wd)
      val currVal = hM(stemWord)(id)
      hM(stemWord)(id) = currVal + 1
    }
  }

  //Calculate weight of page k on page j
  def weight(pageK: Int, pageJ: Int): Double = { // hashMap{key: j_id, value:HashMap{key: k_id, value: Double}}

    //Total number of unique pages that k links to
    val unique = idToLinks.get(pageK).size

    // loop through idtoLinks if unique is 0
    //add this page to every id except itself
    // one page in corpus doesnt have any links --> since it impacts every page

    //TAKE INTO ACCOUNT THAT UNIQUE IS = 0 will impact every other page

    //get title of j
    val titleJ = idToTitle.get(pageJ)

    //here is epsilon
    val epsilon = 0.15

    //If k links to j
    if (idToLinks.get(pageK).contains(titleJ)) {
      (epsilon / n) + ((1 - epsilon) / unique)
    } else {
      epsilon / n
    }
  }

  //Calculate the distance between r and r'
  def distance(previous: mutable.HashMap[Int, Double],
               current: mutable.HashMap[Int, Double]): Double = {
    val sumDifferences = 0.0
    for (element <- previous.values) {
      for (curr <- current.values) {
        sumDifferences + scala.math.pow(curr - element, 2)
      }
    }
    scala.math.sqrt(sumDifferences)
  }

  def pageRank() { //Function pageRank → output: HashMap IdToRank{ key: id, value: Double}
    // hashmap --> key is the page; val is a hashtable (key: page, value: weight))
    var previousR = new mutable.HashMap[Int, Double]
    //array of n zeros //Hashmap (key: id, value: array)
    val currentR = new mutable.HashMap[Int, Double]
    for ((id, _) <- idToTitle) {
      previousR.put(id, 0)
      currentR.put(id, 1 / n)
    }
    while (distance(previousR, currentR) > 0.0001) {
      previousR = currentR
      for (j <- 0 until n) {
        currentR(j) = 0
        for (k <- 0 until n) {
          currentR(j) = currentR(j) + (weight(j, k) * previousR(k))
        }
      }
    }
    //add the value + corresponding page to hashmap
    for ((id, value) <- currentR) {
      idToRank.put(id, value)
    }
  }

  //Relevance Score tf idf here

  //    Perform some sort of computation (addition/multiplication/etc.)
  //    combining TD*IDF and the score from PageRank algorithm => output the final score for each page
  //      (please refer to the roadmap below);
  //    Word relevance score = TD * IDF; PageRank score (see below to calculate score)
  //    Recommended to moving to querier
  //    Multiply PR & tf *idf

  def innerMaxFreq2(): Unit = {
    var currMax = 0.0
    var currWord = ""

    //Calculating Max Frequency
    for ((id, timesMap) <- idToWords) {
      for ((word, numberTimes) <- timesMap) {
        if (numberTimes > currMax) {
          currMax = numberTimes
          currWord = word
        }
      }
      innerMaxFreq.put(id, currMax)
    }
  }

  private val termMap = new HashMap[String, Double]

  def termFrequency(i: String, j: Int): Double = {

    var c = 0.0
    var a = 0.0

    for ((jId, wordsMap) <- idToWords) {
      if (jId == j) {
        for ((key, value) <- wordsMap) {
          c = value
        }
      }
    }

    for ((word, newMap) <- WordstoPage) {
      if (i == word) {
        for ((id, maxFreq) <- newMap) {
          a = maxFreq
        }
      }
    }
    c / a
  }

  def inverseFrequency(i: String): Double = {
    //val n = total number of documents - count number of ids (keys) in hashtable
    //val n_j = number of documents that contain term i (word)
    //for (word in WordsToPage)
    //size of value (hashmap)
    //WordsToDocumentFrequencies.put(word, (id, size)
    val n = 0 //total # of documents
    //populate n
    for (id <- idToTitle) {
      n + 1
    }
    val n_i = 0 // number of docs that contain term i
    // run through list and add one to get amount of docs w term i
    for (word <- WordstoPage) {
      if (i.equals(word._1)) {
        for (innerMap <- word._2) {
          n_i + 1
        }
      }
    }
    log10(n) / log10(n_i)
  }

  //private val idToRelevance = new HashMap[String, HashMap[Int, Double]]
  def relevanceScore(iWord: String, jID: Int): Double = {
    (termFrequency(iWord, jID)) * (inverseFrequency(iWord))
  }

}

object Index {
  def main(args: Array[String]) {
    val Index1 = new Index(args(0))
    //just print ones
    printDocumentFile(args(3), Index1.innerMaxFreq, Index1.idToRank)
    printTitleFile(args(2), Index1.idToTitle)
    printWordsFile(args(1), Index1.WordstoPage)
  }
}
