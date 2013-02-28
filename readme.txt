Versions:
Scala 2.10.0
ScalaTest 2.10-1.9.1

From the root directory:

to compile
> scalac -cp lib/scalatest_2.10-1.9.1.jar -d bin src/*.scala test/*.scala
to run tests
> scala -cp lib/scalatest_2.10-1.9.1.jar org.scalatest.tools.Runner -R bin
to run program
> scala -cp bin RandomSentenceGenerator data/test.txt

======================================================================================

                                    
Sentence Generation Exercise:

The purpose of this exercise is to generate semi-realistic sentences from an existing text.

To do this, we match all pairs of consecutive words in the text with all the words that can follow them.

For example, consider the following short input text:

"I wish I may I wish I might"

In this example, the first word pair, "I wish" appears twice, and is followed by "I" in both cases. The next word pair, "wish I", also appears twice, and is followed by both "may" and "might". The third word pair, "I may", only appears once, where it is followed by "I".

To generate a random sentence from this input text, start with a random word pair (that appears in the text), and see what can follow it. If there is more than one option, choose randomly. Take the last two words of the sentence you have generated, and continue. Stop when there are no possible continuations or after a maximum length of 50 words per sentence.

Here's an example using the above input text:

Say we start with "I may". The only follow up is "I", so now we have:

"I may I"

Now we look for follow-ups to the last pair of our sentence, "may I". The only choice is "wish", and the only choice following "I wish" is "I". So now we have:

"I may I wish I"

For "wish I", we have "may" and "might". Say we randomly chose "might". Now we have:

"I may I wish I might"

There are no follow-ups for "I might", so we are done.

Depending on how the random choices fall, we expect a different result every time.

---------------------------

Your task is to write a short command-line based program that takes the path of an input text file as a parameter and prints out a sentence using this method based on the contents of the input text file, starting from a randomly chosen word pair. 

You should provide some unit tests with your code (100% coverage is not required, but try to cover anything tricky or error-prone). 

Your program should work well for a variety of input text files, including moderately large ones (book length). You may like to try the text of "Animal Farm": http://gutenberg.net.au/ebooks01/0100011.txt

Punctuation should be stripped. Consider whether word pairs should continue past the end of a sentence - the resulting sentences may be more realistic if they don't.

We don't expect every possible edge case to be covered, given the limited time available. There is some room for subjectivity in exactly how you interpret the requirements. Just try to be reasonable - there is no single perfect solution that we are looking for.

Your code may be written in any of the following languages:
Java
Ruby
Groovy
Python
Scala

Please send your code and any dependencies in a zip file, and include instructions on how to compile and run it (and the tests). As this is a small project, we would prefer to be able to run the code without build tools such as Ant, Maven, etc. Shell script is preferred if a build script is needed.