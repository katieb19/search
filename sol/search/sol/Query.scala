package search.sol

import search.src.FileIO

import java.io._
import scala.collection.mutable.HashMap

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
  private val wordsToDocumentFrequencies =
  new HashMap[String, HashMap[Int, Double]]

  //Relevance Score tf idf here

  //    Perform some sort of computation (addition/multiplication/etc.)
  //    combining TD*IDF and the score from PageRank algorithm => output the final score for each page
  //      (please refer to the roadmap below);
  //    Word relevance score = TD * IDF; PageRank score (see below to calculate score)
  //    Recommended to moving to querier
  //    Multiply PR & tf *idf
  //
  //  //helper hashtable
  //  //  def maxHashMap(): HashMap = { //text content each page
  //  //    //for word in document
  //  //    // if (newHashMap.contains(word){
  //  //    // word.value ++
  //  //    // else
  //  //    //newHashMap.put( key = word, value = 1)
  //  //    //end for loop
  //  //
  //  //  }
  //
  //  //populate given idsToMaxFrequencies
  //  def maxFreq(): Unit = { //hashtable of id to title
  //    //each id represents each document
  //    //for (each id in hashmap)
  //    //current documentHashMap = maxHashMap(document)
  //    //idstoMaxFrequencies.put(id, documentHashMap.max)
  //  }
  //
  //  def termFrequency(): Double = {
  //    // val termMap = maxHashMap(input for term freq)
  //    //val c = termMap(word inputted)
  //    //val a = termMap.max
  //    //return final = c/a
  //  }
  //
  //  def inverseFrequency(): Double = {
  //    //val n = total number of documents - count number of ids (keys) in hashtable
  //    //val n_j = number of documents that contain term i (word)
  //    //for (word in WordsToPage)
  //    //size of value (hashmap)
  //    //WordsToDocumentFrequencies.put(word, (id, size)
  //
  //  }
  //
  //  def relevanceScore()
  //  : Integer = { //how to get the tf idf (from max frequencies?)
  //
  //    //val tf = termFrequency()
  //    //val idf = inverseFrequency()
  //    //val pageRank = pageRank() -> how do we get page rank from indexer
  //    //return tf * idf * pageRank // how to call if from the indexer
  //
  //  }

  /**
   * Handles a single query and prints out results
   *
   * @param userQuery - the query text
   */
  private def query(userQuery: String) {
    // TODO : Fill this in
    println("Implement query!")

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

  /*
   * Reads in the text files.
   */
  def readFiles(): Unit = {
    FileIO.readTitles(titleIndex, idsToTitle)
    FileIO.readDocuments(documentIndex, idsToMaxFreqs, idsToPageRank)
    FileIO.readWords(wordIndex, wordsToDocumentFrequencies)
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
