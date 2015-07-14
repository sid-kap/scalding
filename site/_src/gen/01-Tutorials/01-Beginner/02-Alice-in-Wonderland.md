% Alice in Wonderland Tutorial

First, let's import some stuff.

```scala
import scala.io.Source
import com.twitter.scalding._
import com.twitter.scalding.ReplImplicits._
import com.twitter.scalding.ReplImplicitContext._
```

```scala
scala> val alice = Source.fromURL("http://www.gutenberg.org/files/11/11.txt").getLines
alice: Iterator[String] = non-empty iterator
```

Add the line numbers, which we might want later
```scala
scala> val aliceLineNum = alice.zipWithIndex.toList
aliceLineNum: List[(String, Int)] = List((Project Gutenberg's Alice's Adventures in Wonderland, by Lewis Carroll,0), ("",1), (This eBook is for the use of anyone anywhere at no cost and with,2), (almost no restrictions whatsoever.  You may copy it, give it away or,3), (re-use it under the terms of the Project Gutenberg License included,4), (with this eBook or online at www.gutenberg.org,5), ("",6), ("",7), (Title: Alice's Adventures in Wonderland,8), ("",9), (Author: Lewis Carroll,10), ("",11), (Posting Date: June 25, 2008 [EBook #11],12), (Release Date: March, 1994,13), ([Last updated: December 20, 2011],14), ("",15), (Language: English,16), ("",17), (Character set encoding: ASCII,18), ("",19), (*** START OF THIS PROJECT GUTENBERG EBOOK ALICE'S ADVENTURES IN WONDERLAND ***,20), ("",21)...
```

Now for scalding, TypedPipe is the main scalding object representing
your data.

```scala
scala> val alicePipe = TypedPipe.from(aliceLineNum)
alicePipe: com.twitter.scalding.typed.TypedPipe[(String, Int)] = IterablePipe(List((Project Gutenberg's Alice's Adventures in Wonderland, by Lewis Carroll,0), (,1), (This eBook is for the use of anyone anywhere at no cost and with,2), (almost no restrictions whatsoever.  You may copy it, give it away or,3), (re-use it under the terms of the Project Gutenberg License included,4), (with this eBook or online at www.gutenberg.org,5), (,6), (,7), (Title: Alice's Adventures in Wonderland,8), (,9), (Author: Lewis Carroll,10), (,11), (Posting Date: June 25, 2008 [EBook #11],12), (Release Date: March, 1994,13), ([Last updated: December 20, 2011],14), (,15), (Language: English,16), (,17), (Character set encoding: ASCII,18), (,19), (*** START OF THIS PROJECT GUTENBERG EBOOK ALICE'S ADVENTURES IN W...

scala> val aliceWordList = alicePipe.map { line => line._1.split("\\s+") }
aliceWordList: com.twitter.scalding.typed.TypedPipe[Array[String]] = com.twitter.scalding.typed.TypedPipeFactory@18a58466
```

Three things: map, function, tuples
but that's ugly, so we can use tuple matching the be clearer:

```scala
scala> val aliceWordList = alicePipe.map { case (text, lineNum) =>
     |   text.split("\\s+").toList
     | }
aliceWordList: com.twitter.scalding.typed.TypedPipe[List[String]] = com.twitter.scalding.typed.TypedPipeFactory@3f378a65
```

But we want words, not lists of words. We need to flatten!
```scala
scala> val aliceWords = aliceWordList.flatten
aliceWords: com.twitter.scalding.typed.TypedPipe[String] = com.twitter.scalding.typed.TypedPipeFactory@62fa5a78
```

Scala has a common function for this map + flatten == flatMap
```scala
scala> val aliceWords = alicePipe.flatMap { case (text, _) => text.split("\\s+").toList }
aliceWords: com.twitter.scalding.typed.TypedPipe[String] = com.twitter.scalding.typed.TypedPipeFactory@2f0e74e
```

Now lets add a count for each word:
```scala
scala> val aliceWithCount = aliceWords.map { word => (word, 1L) }
aliceWithCount: com.twitter.scalding.typed.TypedPipe[(String, Long)] = com.twitter.scalding.typed.TypedPipeFactory@4e8d7363
```
let's sum them for each word:
```scala
scala> val wordCount = aliceWithCount.group.sum
wordCount: com.twitter.scalding.typed.UnsortedGrouped[String,Long] = IteratorMappedReduce(scala.math.Ordering$String$@2b0b6c98,com.twitter.scalding.typed.TypedPipeFactory@7b06853b,<function2>,None)
```

(We could have also used `.sumByKey`, which is equivalent to `.group.sum`.)

Let's print them to the screen (REPL only):
```scala
scala> wordCount.toIterator.take(100)
res0: Iterator[(String, Long)] = non-empty iterator
```

Let's print just the ones with more that 100 appearances:
```scala
scala> wordCount.filter { case (word, count) => count > 100 }.dump
(,1172)
('I,121)
(Alice,221)
(I,260)
(a,662)
(all,168)
(and,780)
(as,246)
(at,211)
(be,154)
(but,105)
(for,146)
(had,175)
(her,203)
(in,401)
(it,356)
(little,117)
(not,122)
(of,596)
(on,148)
(or,137)
(said,416)
(she,484)
(so,104)
(that,226)
(the,1664)
(they,109)
(this,122)
(to,773)
(very,127)
(was,329)
(with,214)
(you,301)
```

But which is the biggest word?

> Hint: In the Scala REPL, you can turn on `:paste` mode to make it easier to paste multi-line expressions.

```scala
scala> val top10 = { wordCount
     |       .groupAll
     |       .sortBy { case (word, count) => -count }
     |       .take(10) }
top10: com.twitter.scalding.typed.SortedGrouped[Unit,(String, Long)] = ValueSortedReduce(scala.math.Ordering$Unit$@238f81d2,com.twitter.scalding.typed.TypedPipeFactory@2294143b,scala.math.Ordering$$anon$9@27bc3f39,<function2>,Some(1))

scala> top10.dump
((),(the,1664))
((),(,1172))
((),(and,780))
((),(to,773))
((),(a,662))
((),(of,596))
((),(she,484))
((),(said,416))
((),(in,401))
((),(it,356))
```

Where is Alice? What is with the ()?

```scala
scala> val top20 = { wordCount
     |       .groupAll
     |       .sortBy { case (word, count) => -count }
     |       .take(20)
     |       .values } // ignore the ()-all key
top20: com.twitter.scalding.typed.TypedPipe[(String, Long)] = com.twitter.scalding.typed.TypedPipeFactory@416dd5de

scala> top20.dump
(the,1664)
(,1172)
(and,780)
(to,773)
(a,662)
(of,596)
(she,484)
(said,416)
(in,401)
(it,356)
(was,329)
(you,301)
(I,260)
(as,246)
(that,226)
(Alice,221)
(with,214)
(at,211)
(her,203)
(had,175)
```

There she is!

Now, suppose we want to know the last line on which each word appears.

How do we solve this?
First, we generate `(word, lineNum)` pairs by  flatmapping each line of words to a list of `(word, lineNum)` pairs.

```scala
scala> val wordLine = alicePipe.flatMap { case (text, line) =>
     |    text.split("\\s+").toList.map { word => (word, line) }
     |  }
wordLine: com.twitter.scalding.typed.TypedPipe[(String, Int)] = com.twitter.scalding.typed.TypedPipeFactory@68250325
```

Next, we group the pairs on the word, and take the max line number for each group.

> See all the functions on grouped things here:
> [http://twitter.github.io/scalding/#com.twitter.scalding.typed.Grouped](http://twitter.github.io/scalding/#com.twitter.scalding.typed.Grouped)

```scala
scala> val lastLine = wordLine.group.max
lastLine: com.twitter.scalding.typed.UnsortedGrouped[String,Int] = IteratorMappedReduce(scala.math.Ordering$String$@2b0b6c98,com.twitter.scalding.typed.TypedPipeFactory@4e611cb2,<function2>,None)
```

Finally, we lookup the words from the initial line:

> By the way: `lastLine.swap` is equivalent to `lastLine.map { case (word, lastLine) => (lastLine, word) }`

```scala
scala> val words = {
     |   lastLine.map { case (word, lastLine) => (lastLine, word) }
     |           .group
     |           .join(alicePipe.swap.group)
     | }
words: com.twitter.scalding.typed.CoGrouped[Int,(String, String)] = com.twitter.scalding.typed.CoGroupable$$anon$3@45b02f9

scala> println(words.toIterator.take(30).mkString("\n"))
(8,(Title:,Title: Alice's Adventures in Wonderland))
(10,(Author:,Author: Lewis Carroll))
(12,(#11],Posting Date: June 25, 2008 [EBook #11]))
(12,(2008,Posting Date: June 25, 2008 [EBook #11]))
(12,(25,,Posting Date: June 25, 2008 [EBook #11]))
(12,(June,Posting Date: June 25, 2008 [EBook #11]))
(12,(Posting,Posting Date: June 25, 2008 [EBook #11]))
(12,([EBook,Posting Date: June 25, 2008 [EBook #11]))
(13,(1994,Release Date: March, 1994))
(13,(Date:,Release Date: March, 1994))
(13,(Release,Release Date: March, 1994))
(14,(20,,[Last updated: December 20, 2011]))
(14,(2011],[Last updated: December 20, 2011]))
(14,(December,[Last updated: December 20, 2011]))
(14,([Last,[Last updated: December 20, 2011]))
(14,(updated:,[Last updated: December 20, 2011]))
(16,(Language:,Language: English))
(18,(ASCII,Character set encoding: ASCII))
(18,(Character,Character set encoding: ASCII))
(18,(encoding:,Character set encoding: ASCII))
(20,(START,*** START OF THIS PROJECT GUTENBERG EBOOK ALICE'S ADVENTURES IN WONDERLAND ***))
(35,(3.0,THE MILLENNIUM FULCRUM EDITION 3.0))
(35,(EDITION,THE MILLENNIUM FULCRUM EDITION 3.0))
(35,(FULCRUM,THE MILLENNIUM FULCRUM EDITION 3.0))
(35,(MILLENNIUM,THE MILLENNIUM FULCRUM EDITION 3.0))
(40,(Down,CHAPTER I. Down the Rabbit-Hole))
(40,(I.,CHAPTER I. Down the Rabbit-Hole))
(40,(Rabbit-Hole,CHAPTER I. Down the Rabbit-Hole))
(43,(do:,bank, and of having nothing to do: once or twice she had peeped into the))
(43,(twice,bank, and of having nothing to do: once or twice she had peeped into the))
```

That's it.
You have learned the basics:
TypedPipe, map/flatMap/filter
groups do reduce/join: max, sum, join, take, sortBy
