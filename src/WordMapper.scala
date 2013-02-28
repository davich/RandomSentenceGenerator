import scala.collection.mutable._
object WordMapper {
  private val endOfSentence = "[\\.\\?\\!]+"    
  private val wordOrEndOfSentenceRegex = ("[\\w\\']+|" + endOfSentence).r
  private val endOfSentenceRegex = endOfSentence.r
  
  
  def mapWordPaths(s:String) = {
    val iterator = wordOrEndOfSentenceRegex.findAllIn(s).sliding(3)
    val wordPaths:Map[(String, String), Array[String]] = Map()

    while (iterator.hasNext) {
      val tokens = iterator.next()
      val includesEndOfSentence = tokens.exists(s => endOfSentenceRegex.pattern.matcher(s).matches)
      if (tokens.size == 3 && !includesEndOfSentence) {
        val words = cleanWords(tokens)
        if (wordPaths.contains((words(0),words(1)))) {
          // Design decision: make the array of possible next words non-unique. This way, words that 
          // commonly follow other words have a higher chance of being chosen next. This should
          // produce more realistic text. Downside: Higher memory usage. I assume this won't be a problem. 
          wordPaths((words(0),words(1))) = Array.concat(wordPaths(words(0),words(1)), Array(words(2)))
        } else {
          wordPaths += ((words(0),words(1)) -> Array(words(2)))
        }
      }
    }
    wordPaths
  }

  def cleanWords(words : List[String]) = {
    words.map(_.toLowerCase().replaceAll("^[^\\w]+", "").replaceAll("[^\\w]+$", ""))
  }
}