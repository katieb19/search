package search.sol
import scala.collection.mutable
import scala.util.matching.Regex
import scala.collection.mutable.HashMap
import scala.xml.{Node, NodeSeq}
import search.src.StopWords.isStopWord

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
  private val idToWords = new HashMap()[Int, Array[String]] //string word --> hashmap of int (id) to relevance (double
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
      for (match <- matchesList){
        //if word is there (some or none)
        if (!isStopWord(match)){
          match.stem()
        }
      }



      //val copyListL List = matchesList.filter(!non stop word).map(stem(pageSeg))



    }


    //val pagesFiltered = pageSeq.filter()



    //delete first words e.g. wathington?

  }


}






}

object Index {
  def main(args: Array[String]) {
    // TODO : Implement!
    System.out.println("Not implemented yet!")
  }
}
