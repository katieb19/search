package search.sol

import search.src.FileIO.{printTitleFile, printWordsFile}
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
  //Hash tables
  private var WordstoPage = new HashMap()[String, HashMap[Int, Double]] //string word -->[int (id) -> max Freq]
  private val idToLinks = new HashMap()[Int, mutable.HashSet[String]] //id to linked pages
  private val idToTitle = new HashMap()[Int, String]
  private val idToRank = new HashMap()[Int, Double]
  private val idToWords = new HashMap()[Int, HashMap[String, Int]] //id to [Words, # times it appears]


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
      for ((id, title)<- idToTitle){
        idToWords.put(id, HashMap.empty[String, Int])
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
      val matchesList = matchesIterator.toList.map { aMatch => aMatch.matched }

      //for loop
      for (m <- matchesList) {
        // if m is a link (regex)
        if (m.matches("\\[\\[[^\\[]+?\\]\\]")) {
          //NOTE TO SELF: need to check that word being added doesnt include [] in it
          //        then populate the id to link hashmap
          //        then check if link is not category format
          //             if it isnt then populate word to freq table
          //case that link doesnt have category or |
          if (!m.contains("|") | !m.contains("Category:")) {
            val newSet = new mutable.HashSet[String]()
            //take out square brackets//DO WE NEED TO AND HOW?
            // Use regex and then it ll pick up all of the words in list to add in loop through
            newSet.add(m)
            idToLinks(id.text.toInt) = newSet
            addFunWord(id.text.toInt, m, WordstoPage)


          //case that link has Presidents|Washington
          else if (m.contains("|")) {
            //populate idtolink
            val newSet = new mutable.HashSet[String]()
            newSet.add(m) //NEED TO ADD STUFF BEFORE | (how?)
            idToLinks(id.text.toInt) = newSet
            //add word after |
            val regex1 = m.split("|") //how to remove the words after | and |?????
            //populate add fun word
            addFunWord(id.text.toInt, regex1[1], WordstoPage)


          }
          //else if category [Category: Computer Science] --> Category: Computer Science
          // category, computer, science --> added to word list
          else {
            //add to id to link (DO WE NEED TO ADD TO ID TO WORDS???) //Im thinking of regex
            val newSet = new mutable.HashSet[String]()
            newSet.add(m)
            idToLinks(id.text.toInt) = newSet
          }
        }
        // else check if word isnt a stop word
        else if (!isStopWord(m)) {
          //        then take the stem of said word
          val stemWord = stem(m)
          //        then populate word to freq table
          addFunWord(id.text.toInt, stemWord, WordstoPage)
        }

          //populate idToWords
          for ((id, newMap) <- idToWords){
            var currValue = 0
            for (mapKey <- newMap.keys){
              if (mapKey == m){
                currValue += 1
                mapKey -> currValue
              } else {
                currValue += 1
                newMap.put(m, currValue)
              }
            }
          }



        }

      }
    }
  }

  def addFunWord(id: Int, wd: String, hM: mutable.HashMap[String, mutable.HashMap[Int, Double]]): Unit = {
    //adds a word to a hashmap
    if (!hM.contains(wd)) {
      val addHash = new mutable.HashMap()[Int, Double]
      addHash(id) = 1
      hM += (wd -> addHash)
    }
    else {
      val currVal = hM(wd)(id)
      hM(wd)(id) = currVal + 1
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
    }
    else {
      epsilon / n
    }
  }


  //Calculate the distance between r and r'
  def distance(previous: mutable.HashMap[Int, Double], current: mutable.HashMap[Int, Double]): Double = {
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
    var previousR = new mutable.HashMap()[Int, Double]
    //array of n zeros //Hashmap (key: id, value: array)
    val currentR = new mutable.HashMap()[Int, Double]
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

  //helper id -> max frequencies
  private var innerMaxFreq = new HashMap[Int, Int]

  def innerMaxFreq(): Unit = {
    //Values to be added to WordsToPage
    var currMax = 0
    var currWord = ""

    //Calculating Max Frequency
    for ((id, timesMap) <- idToWords){
      for ((word, numberTimes) <- timesMap){
        if (numberTimes > currMax){
          currMax = numberTimes
          currWord = word
        }
      }
      innerMaxFreq.put(id, currMax)
    }
  }

  //populate given idsToMaxFrequencies
  def maxFreq(): Unit = { //hashtable of id to title
    for ((word, innerMap) <- WordstoPage) {
      for ((innerId, value) <- innerMap) {
        for ((id, maxFreq) <- innerMaxFreq) {
          if (innerId == id) {
            value = maxFreq //it is a VAR UGHHHH
          }
        }
      }
    }
  }

  private val termMap = new HashMap[String, Double]

  def termFrequency(i: String, j: Int ): Double = {

    var c = 0.0
    var a = 0.0

    for ((jId, wordsMap) <- idToWords) {
      if (jId == j) {
        for ((key, value) <- wordsMap) {
          c = value
        }
      }
    }

    for ((word, newMap) <- WordstoPage){
      if (i == word){
        for ((id, maxFreq) <- newMap){
          a = maxFreq
        }
      }
    }
    c/a
  }

  def inverseFrequency(): Double = {
    //val n = total number of documents - count number of ids (keys) in hashtable
    //val n_j = number of documents that contain term i (word)
    //for (word in WordsToPage)
    //size of value (hashmap)
    //WordsToDocumentFrequencies.put(word, (id, size)

  }








}

object Index {
  def main(args: Array[String]) {
    val Index1 = new Index(args(0))
    //just print ones
    printDocumentFile("documents.txt", Index1.WordstoPage, Index1.WordstoPage) //Clark said it should be ok (?
    printTitleFile("titles.txt", Index1.idToTitle)
    printWordsFile("words.txt", Index1.WordstoPage)
    System.out.println("Not implemented yet!")
  }
}
