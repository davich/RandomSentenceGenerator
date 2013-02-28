import org.scalatest._
import scala.collection.mutable._

class RandomSentenceGeneratorSpec extends FlatSpec {
  
  behavior of "getFileContents"
  it should "read and return a file's contents" in {
    val result = RandomSentenceGenerator.getFileContents("data/small.txt")
    assert(result == "this is a small file")
  }
  
  behavior of "generateSentence"
  it should "not generate a sentence more than 50 words long" in {
    val map:Map[(String, String), Array[String]] = Map()
    map(("try", "this")) =  Array("now")
    map(("this", "now")) = Array("try")
    map(("now", "try")) = Array("this")
    val result = RandomSentenceGenerator.generateSentence(map)
    val wordCount = "[\\w\\']+".r.findAllIn(result).size
    assert(wordCount == 50)
  }
  
  it should "stop when there are no more paths to take" in {
    val map:Map[(String, String), Array[String]] = Map()
    map(("try", "this")) =  Array("now")
    map(("this", "now")) = Array("try")
    val result = RandomSentenceGenerator.generateSentence(map)
    val wordCount = "[\\w\\']+".r.findAllIn(result).size
    assert(wordCount == 3 || wordCount == 4)
  }
  
  it should "handle empty file" in {
    val map:Map[(String, String), Array[String]] = Map()
    val result = RandomSentenceGenerator.generateSentence(map)
    assert(result.size == 0)
  }
  
  "randomKey" should "pick a key at random from the list" in {
    val map:Map[(String, String), Array[String]] = Map()
    map(("to", "the")) =  Array("chopper")
    map(("i'll", "be")) = Array("back")
    map(("not", "a")) = Array("tumor")
    
    val key = RandomSentenceGenerator.randomKey(map)
    assert(map.contains(key))
  }

  "randomNextWord" should "pick a next word at random from multiple options" in {
    val map:Map[(String, String), Array[String]] = Map()
    val key = ("check", "this")
    val nextWords = Array("function", "method", "stuff", "problem", "solution", "answer", "out", "yo", "it's", "momma")
    map(key) = nextWords
    val result1 = RandomSentenceGenerator.randomNextWord(map, key)
    assert(nextWords.contains(result1))
    
    // It's always tricky to test if something is random. 
    // This test might incorrectly fail 1 in 100,000,000,000 times
    // which I think is reasonable. 
    // (if run a million times a day, it should fail once every 274 years)
    var i = 0
    var difference = false
    for (i <- 1 to 10
        if !difference) {
      if (result1 != RandomSentenceGenerator.randomNextWord(map, key)) {
        difference = true
      }
    }
    assert(difference)
  }
}