import org.scalatest._
import scala.collection.mutable._

class RandomSentenceGeneratorSpec extends FlatSpec {
  
  behavior of "getFileContents"
  it should "read and return a file's contents" in {
    val result = RandomSentenceGenerator.getFileContents("data/small.txt")
    assert(result == "this is a small file")
  }

  behavior of "mapWordPaths"
  it should "handle basic mapping and making lower case" in {
    val result:Map[(String, String), Array[String]] = RandomSentenceGenerator.mapWordPaths("THIs is A String")
    assert(result.size == 2)
    assert(result("this", "is").deep == Array("a").deep)
    assert(result("is", "a").deep == Array("string").deep)
  }
  
  it should "handle empty string" in {
    assert(RandomSentenceGenerator.mapWordPaths("").size == 0)
  }
  
  it should "handle not enough strings to make a map entry" in {
    assert(RandomSentenceGenerator.mapWordPaths("one").size == 0)
    assert(RandomSentenceGenerator.mapWordPaths("one two").size == 0)
  }
  
  it should "remove punctuation except apostropties" in {
    val result:Map[(String, String), Array[String]] = RandomSentenceGenerator.mapWordPaths("this/,> isn't an - integer $$")
    assert(result.size == 2)
    assert(result("this", "isn't").deep == Array("an").deep)
    assert(result("isn't", "an").deep == Array("integer").deep)
  }
  
  it should "not map words over sentence boundries" in {
    val result:Map[(String, String), Array[String]] = RandomSentenceGenerator.mapWordPaths("why hello. how are you? good")
    assert(result.size == 1)
    assert(result("how", "are").deep == Array("you").deep)
  }
  
  it should "remove single quotes around words but leave in apostrophies" in {
    // note: This still removes trailing apostrophies from words (eg. his parents' house).
    // Considered out of scope of the exercise to distinguish between closing single quotes and 
    // trailing apostrophies
    val result:Map[(String, String), Array[String]] = RandomSentenceGenerator.mapWordPaths("'this' 'isn't cool'")
    assert(result.size == 1)
    assert(result("this", "isn't").deep == Array("cool").deep)
  }
  
  it should "handle multiple word occurances" in {
    val result:Map[(String, String), Array[String]] = RandomSentenceGenerator.mapWordPaths("to be or not to be or something")
    assert(result("be", "or").deep == Array("not", "something").deep)
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