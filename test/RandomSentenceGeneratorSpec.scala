import org.scalatest._
import scala.collection.mutable._

// windows
// scalac -cp lib\scalatest_2.10-1.9.1.jar;bin -d bin src\RandomSentenceGenerator.scala test\RandomSentenceGeneratorSpec.scala
// scala -cp lib\scalatest_2.10-1.9.1.jar;bin org.scalatest.run RandomSentenceGeneratorSpec

// other
// scalac -cp lib/scalatest_2.10-1.9.1.jar:bin -d bin src/RandomSentenceGenerator.scala test/RandomSentenceGeneratorSpec.scala
// scala -cp lib/scalatest_2.10-1.9.1.jar:bin org.scalatest.run RandomSentenceGeneratorSpec


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
  
  it should "remove punctuation except apostropties" in {
    val result:Map[(String, String), Array[String]] = RandomSentenceGenerator.mapWordPaths("this... isn't an - integer $$")
    assert(result.size == 2)
    assert(result("this", "isn't").deep == Array("an").deep)
    assert(result("isn't", "an").deep == Array("integer").deep)
  }
  
  it should "handle multiple word occurances" in {
    val result:Map[(String, String), Array[String]] = RandomSentenceGenerator.mapWordPaths("to be or not to be or something")
    assert(result("be", "or").deep == Array("not", "something").deep)
  }
  
  
  
  it should "generate random sentence" in {
    RandomSentenceGenerator.main(Array("data/test.txt"))
  }
}