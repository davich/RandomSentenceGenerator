import scala.collection.mutable._
import scala.io.Source
import java.io.File
import scala.util.Random

object RandomSentenceGenerator {
  private val rand = new Random(System.currentTimeMillis())
  private val maxWords = 50
  def main(args: Array[String]): Unit = {
    if (args.size != 1) {
      println("Invalid arguments: " + args.mkString(" "))
      return
    }
    val filename = args(0)
    if (new File(filename).exists == false) {
      println("Unknown file: " + filename)
      return
    }
      
    val fileContents = getFileContents(filename)
    val wordPaths = mapWordPaths(fileContents)
    println(generateSentence(wordPaths))
  }
  
  def getFileContents(filename: String) = {
    val source = Source.fromFile(filename)
    val lines = source.mkString
    source.close
    lines
  }
  
  def mapWordPaths(s:String) = {
    val endOfSentence = "[\\.\\?\\!]+"    
    val wordOrEndOfSentence = "[\\w\\']+|" + endOfSentence
    val iterator = wordOrEndOfSentence.r.findAllIn(s).sliding(3)
    val endOfSentenceRegex = endOfSentence.r
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
  
  def generateSentence(wordPaths:Map[(String, String), Array[String]]):String = {
    if (wordPaths.size == 0) {
      return ""
    }
    var key = randomKey(wordPaths)
    val result = ListBuffer(key._1, key._2)
    while (wordPaths.contains(key) &&
        wordPaths(key).size > 0 &&
        result.size < maxWords) {
      val nextWord = randomNextWord(wordPaths, key)
      result += nextWord
      key = (key._2, nextWord)
    }
    result.mkString(" ")
  }
  
  def randomKey(wordPaths:Map[(String, String), Array[String]]) = {
    val keys = wordPaths.keys.toList
    keys(rand.nextInt(keys.length))
  }
  
  def randomNextWord(wordPaths:Map[(String, String), Array[String]], key:(String, String)) = {
    val possibleNextWords = wordPaths(key)
    possibleNextWords(rand.nextInt(possibleNextWords.length))
  }
}