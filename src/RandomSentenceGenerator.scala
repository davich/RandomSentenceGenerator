import scala.collection.mutable._
import scala.io.Source
import scala.util.Random

object RandomSentenceGenerator {
  private val rand = new Random(System.currentTimeMillis())
  def main(args: Array[String]): Unit = {
    if (args.size != 1) {
      println("Invalid arguments: " + args.mkString(" "))
      return
    }
    val filename = args(0)
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
    val iterator = "[\\w\\']+|[\\.\\?\\!]+".r.findAllIn(s).sliding(3)
    val wordPaths:Map[(String, String), Array[String]] = Map()
    val endOfSentenceRegex = "[\\.\\?\\!]+".r

    while (iterator.hasNext) {
      val tokens = iterator.next()
      val includesEndOfSentence = tokens.exists(s => endOfSentenceRegex.pattern.matcher(s).matches)
      if (!includesEndOfSentence) {
        val words = cleanWords(tokens)
        val word1 = words(0)
        val word2 = words(1)
        val word3 = words(2)
        if (wordPaths.contains((word1,word2))) {
          wordPaths((word1,word2)) = Array.concat(wordPaths(word1,word2), Array(word3))
        } else {
          wordPaths += ((word1,word2) -> Array(word3))
        }
      }
    }
    wordPaths
  }
  def cleanWords(words : List[String]) = {
    words.map(_.toLowerCase().replaceAll("^[^\\w]+", "").replaceAll("[^\\w]+$", ""))
  }
  
  def generateSentence(wordPaths:Map[(String, String), Array[String]]) = {
    var key = randomKey(wordPaths)
    val result = ListBuffer(key._1, key._2)
    while (wordPaths.contains(key) &&
        wordPaths(key).size > 0 &&
        result.size < 50) {
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