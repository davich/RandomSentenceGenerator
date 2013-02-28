import org.scalatest._
import scala.collection.mutable._

class WordMapperSpec extends FlatSpec {
  behavior of "mapWordPaths"
  
  it should "handle basic mapping and making lower case" in {
    val result:Map[(String, String), Array[String]] = WordMapper.mapWordPaths("THIs is A String")
    assert(result.size == 2)
    assert(result("this", "is").deep == Array("a").deep)
    assert(result("is", "a").deep == Array("string").deep)
  }
  
  it should "handle empty string" in {
    assert(WordMapper.mapWordPaths("").size == 0)
  }
  
  it should "handle not enough strings to make a map entry" in {
    assert(WordMapper.mapWordPaths("one").size == 0)
    assert(WordMapper.mapWordPaths("one two").size == 0)
  }
  
  it should "remove punctuation except apostropties" in {
    val result:Map[(String, String), Array[String]] = WordMapper.mapWordPaths("this/,> isn't an - integer $$")
    assert(result.size == 2)
    assert(result("this", "isn't").deep == Array("an").deep)
    assert(result("isn't", "an").deep == Array("integer").deep)
  }
  
  it should "not map words over sentence boundries" in {
    val result:Map[(String, String), Array[String]] = WordMapper.mapWordPaths("why hello. how are you? good")
    assert(result.size == 1)
    assert(result("how", "are").deep == Array("you").deep)
  }
  
  it should "remove single quotes around words but leave in apostrophies" in {
    // note: This still removes trailing apostrophies from words (eg. his parents' house).
    // Considered out of scope of the exercise to distinguish between closing single quotes and 
    // trailing apostrophies
    val result:Map[(String, String), Array[String]] = WordMapper.mapWordPaths("'this' 'isn't cool'")
    assert(result.size == 1)
    assert(result("this", "isn't").deep == Array("cool").deep)
  }
  
  it should "handle multiple word occurances" in {
    val result:Map[(String, String), Array[String]] = WordMapper.mapWordPaths("to be or not to be or something")
    assert(result("be", "or").deep == Array("not", "something").deep)
  }
}