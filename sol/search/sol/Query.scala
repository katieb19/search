package search.sol

import search.src.FileIO
import search.src.PorterStemmer.stem
import search.src.StopWords.isStopWord

import java.io._
import scala.collection.mutable
import scala.collection.mutable.HashMap
import scala.math.log10

/**
 * Represents a query REPL built off of a specified index
 *
 * @param titleIndex    - the filename of the title index
 * @param documentIndex - the filename of the document index
 * @param wordIndex     - the filename of the word index
 * @param usePageRank   - true if page rank is to be incorporated into scoring
 */
class Query(titleIndex: String,
            documentIndex: String,
            wordIndex: String,
            usePageRank: Boolean) {

  // Maps the document ids to the title for each document
  private val idsToTitle = new HashMap[Int, String]

  // Maps the document ids to the euclidean normalization for each document
  private val idsToMaxFreqs = new HashMap[Int, Double]

  // Maps the document ids to the page rank for each document
  private val idsToPageRank = new HashMap[Int, Double]

  // Maps each word to a map of document IDs and frequencies of documents that
  // contain that word
  private val wordsToDocumentFrequencies = {
    new HashMap[String, HashMap[Int, Double]]
  }

  /**
   * Handles a single query and prints out results
   *
   * @param userQuery - the query text
   */
  private def query(userQuery: String) {
    if (usePageRank) {
      withPageRank(userQuery.toLowerCase())
    }
    withoutPageRank(userQuery.toLowerCase())
  }

  /**
   * Calculates the score of the query without Page Rank
   *
   * @param query - takes in a user input
   */
  def withoutPageRank(query: String): Unit = {

    //Regex
    val stringSplit = query.split(" ")

    //Storing of all non Stop Words
    var nonStopWords = new mutable.HashSet[String]()

    //Populating nonStopWords
    for (word <- stringSplit) {
      if (!isStopWord(word)) {
        nonStopWords += stem(word)
      }
    }

    //Storing ids to scores
    var idsToScores = new mutable.HashMap[Int, Double]

    //Populating IdsToScores
    for (wd <- nonStopWords) {
      val inner = wordsToDocumentFrequencies(wd)
      for ((id, freq) <- inner) {
        val sumMax = termFrequency(wd, id) * inverseFrequency(wd)
        idsToScores.put(id, sumMax)
      }
    }

    //Converting to Array and Sorting
    val mapToTuples = idsToScores.toArray
    val sortedArray =
      mapToTuples.sortWith((first, second) => first._2 > second._2)

    //Populates Array to be printed
    val n = sortedArray.size
    val funArray = new Array[Int](n)
    var index = 0
    for (key <- sortedArray) {
      funArray(index) = key._1
      index += 1
    }

    printResults(funArray)
  }

  /**
   * Calculates the score of the query with Page Rank
   *
   * @param query - takes in a user input
   */
  def withPageRank(query: String): Unit = {
    //Regex
    val stringSplit = query.split(" ")

    //Storing of all non Stop Words
    var nonStopWords = new mutable.HashSet[String]()

    //Populating nonStopWords
    for (word <- stringSplit) {
      if (!isStopWord(word)) {
        nonStopWords += stem(word)
      }
    }

    //Storing ids to scores
    var idsToScores = new mutable.HashMap[Int, Double]

    //Populating IdsToScores
    for (wd <- nonStopWords) {
      val inner = wordsToDocumentFrequencies(wd)
      for ((id, freq) <- inner) {
        val sumMax = relevanceScore(wd, id)
        idsToScores.put(id, idsToScores(id) + sumMax)
      }
    }

    //Converting to Array and Sorting
    val mapToTuples = idsToScores.toArray
    val sortedArray =
      mapToTuples.sortWith((first, second) => first._2 > second._2)

    //Populates Array to be printed
    val n = sortedArray.size
    val funArray = new Array[Int](n)
    var index = 0
    for (key <- sortedArray) {
      funArray(index) = key._1
      index += 1
    }

    printResults(funArray)
  }

  /**
   * Format and print up to 10 results from the results list
   *
   * @param results - an array of all results to be printed
   */
  private def printResults(results: Array[Int]) {
    for (i <- 0 until Math.min(10, results.size)) {
      println("\t" + (i + 1) + " " + idsToTitle(results(i)))
    }
  }

  /**
   * Reads in the text files.
   */
  def readFiles(): Unit = {
    FileIO.readTitles(titleIndex, idsToTitle)
    FileIO.readDocuments(documentIndex, idsToMaxFreqs, idsToPageRank)
    FileIO.readWords(wordIndex, wordsToDocumentFrequencies)
  }

  /**
   * Calculates the tf
   *
   * @param i : String representing word
   * @param j : Int representing document ID
   * @return tf
   */
  def termFrequency(i: String, j: Int): Double = {

    //Variables
    var c = 0.0
    var a = 0.0

    //Calculating c
    for ((id, totalTimes) <- idsToMaxFreqs) {
      if (j == id) {
        c = totalTimes
      }
    }

    //Calculating a
    for ((word, newMap) <- wordsToDocumentFrequencies) {
      if (i == word) {
        for ((_, maxFreq) <- newMap) {
          a = maxFreq
        }
      }
    }

    //Term Frequency
    c / a
  }

  /**
   * Calculates the tf
   *
   * @param i : String representing word
   * @return idf
   */
  def inverseFrequency(i: String): Double = {

    //total # of documents
    val n = idsToTitle.size.toDouble

    // number of docs that contain term i
    var n_i = 0.0

    // run through list and add one to get amount of docs w term i
    for ((word, map) <- wordsToDocumentFrequencies) {
      if (i.equals(word)) {
        for (innerMap <- map) {
          n_i += 1.0
        }
      }
    }

    //Return idf
    log10(n) / log10(n_i)
  }

  /**
   * Calculates the relevance score given a word and a document id
   *
   * @param iWord : String representing word
   * @param jID   : Int representing document id
   * @return relevance score
   */
  def relevanceScore(iWord: String, jID: Int): Double = {
    var score = (termFrequency(iWord, jID)) * (inverseFrequency(iWord))

    for ((id, rank) <- idsToPageRank) {
      if (id == jID) {
        score *= rank
      }
    }
    score
  }

  /**
   * Starts the read and print loop for queries
   */
  def run() {
    val inputReader = new BufferedReader(new InputStreamReader(System.in))

    // Print the first query prompt and read the first line of input
    print("search> ")
    var userQuery = inputReader.readLine()

    // Loop until there are no more input lines (EOF is reached)
    while (userQuery != null) {
      // If ":quit" is reached, exit the loop
      if (userQuery == ":quit") {
        inputReader.close()
        return
      }

      // Handle the query for the single line of input
      query(userQuery)

      // Print next query prompt and read next line of input
      print("search> ")
      userQuery = inputReader.readLine()
    }

    inputReader.close()
  }
}

object Query {
  def main(args: Array[String]) {
    try {
      // Run queries with page rank
      var pageRank = false
      var titleIndex = 0
      var docIndex = 1
      var wordIndex = 2
      if (args.size == 4 && args(0) == "--pagerank") {
        pageRank = true;
        titleIndex = 1
        docIndex = 2
        wordIndex = 3
      } else if (args.size != 3) {
        println(
          "Incorrect arguments. Please use [--pagerank] <titleIndex> "
            + "<documentIndex> <wordIndex>"
        )
        System.exit(1)
      }
      val query: Query =
        new Query(args(titleIndex), args(docIndex), args(wordIndex), pageRank)
      query.readFiles()
      query.run()
    } catch {
      case _: FileNotFoundException =>
        println("One (or more) of the files were not found")
      case _: IOException => println("Error: IO Exception")
    }
  }
}
