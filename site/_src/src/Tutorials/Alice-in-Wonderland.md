% Alice in Wonderland Tutorial

First, let's import some stuff.

```tut:silent
import scala.io.Source
import com.twitter.scalding._
import com.twitter.scalding.ReplImplicits._
import com.twitter.scalding.ReplImplicitContext._
```

```tut
//val alice = Source.fromURL("http://www.gutenberg.org/files/11/11.txt").getLines
val alice = Source.fromFile("/home/sidharth/workspace/site/_src/src/11.txt").getLines
```

Add the line numbers, which we might want later
```tut
val aliceLineNum = alice.zipWithIndex.toList
```

Now for scalding, TypedPipe is the main scalding object representing
your data.

```tut
val alicePipe = TypedPipe.from(aliceLineNum)

val aliceWordList = alicePipe.map { line => line._1.split("\\s+").toList }
```

Three things: map, function, tuples
but that's ugly, so we can use tuple matching the be clearer:

```tut
val aliceWordList = alicePipe.map { case (text, lineno) =>
  text.split("\\s+").toList
}
```

But we want words, not lists of words. We need to flatten!
```tut
val aliceWords = aliceWordList.flatten
```

Scala has a common function for this map + flatten == flatMap
```tut
val aliceWords = alicePipe.flatMap { case (text, _) => text.split("\\s+").toList }
```

Now lets add a count for each word:
```tut
val aliceWithCount = aliceWords.map { word => (word, 1L) }
```
let's sum them for each word:
```tut
val wordCount = aliceWithCount.group.sum
```

or: .group.sum == .sumByKey

let's print them to the screen (REPL only)
```tut
wordCount.toIterator.take(100)
```

Let's print just the ones with more that 100 appearances:
```tut
wordCount.filter { case (word, count) => count > 100 }.dump
```

but which is the biggest word?
use, :paste to put multi-line expressions
```tut
val top10 = (wordCount
      .groupAll
      .sortBy { case (word, count) => -count }
      .take(10))

top10.dump
```

Where is Alice? What is with the ()?
use, :paste to put multi-line expressions

```tut
val top20 = (wordCount
      .groupAll
      .sortBy { case (word, count) => -count }
      .take(20)
      .values) // ignore the ()-all key

top20.dump
```

there she is!

what is the last line, on which each word appears?

How to solve this?
(flat)map text to (word, lineno) pairs
for each word, take the maximum line num
then join the line number to the original input

```tut
val wordLine = alicePipe.flatMap { case (text, line) =>
   text.split("\\s+").toList.map { word => (word, line) }
 }
```

Take the max
see all the functions on grouped things here:
http://twitter.github.io/scalding/#com.twitter.scalding.typed.Grouped

```tut
val lastLine = wordLine.group.max
```

now lookup the initial line:
same as .swap, by the way

```tut
println(lastLine.map { case (word, lastLine) => (lastLine, word) }
  .group
  .join(alicePipe.swap.group)
  .toIterator
  .take(30).mkString("\n"))
```

That's it.
You have learned the basics:
TypedPipe, map/flatMap/filter
groups do reduce/join: max, sum, join, take, sortBy
