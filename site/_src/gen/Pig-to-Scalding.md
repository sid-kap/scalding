% Pig to Scalding

This pages intends to help Pig users to learn Scalding by listing corresponding statements and basic Scala knowledge.
You should also take a look at the [tutorial](https://github.com/twitter/scalding/blob/develop/tutorial/TypedTutorial.scala).

## LOAD
Pig:
```
A = LOAD 'foo'
```
Scalding:
```scala
// The TextLine source splits the input by lines.
val textSource = TextLine(args("input"))
// Create a type-safe pipe from the TextLine.
val lines: TypedPipe[String] = TypedPipe.from[String](textSource)
```

## STORE
Pig:
```
STORE B INTO 'bar'
```
Scalding:
```scala
b.write(TypedTsv[String](args("output")))
```

## FOREACH
Pig:
```
B = FOREACH A GENERATE /* expression */
```
Scalding:
```scala
val b = a.map((t) => /* expression */)
```

## FILTER
Pig:
```
B = FILTER A BY foo == 0
```
Scalding:
```scala
a.filter{ case (foo, bar) => foo == 0 } // using pattern matching to name elements of a tuple
// if you don't need to name an element you can use the _ wildcard instead
a.filter{ case (foo, _) => foo == 0 }
```

## FOREACH A GENERATE FLATTEN(...)
in Scalding the use of flatMap is similar to the following in Pig:
```
B = FOREACH A GENERATE FLATTEN(Tokenize(text))
```
in Scalding:
```scala
def tokenize(s: String) = s.split("\\s+").toList
b = a.flatMap(tokenize(_))
// which produces the same result as:
b = a.map(tokenize(_)).flatten()
// and the same as
b = a.map(tokenize(_)).flatten // empty parens are usually omitted
```

## Aggregating
Pig:
```
B = FOREACH (GROUP A BY $0) GENERATE COUNT(A)
```
Scalding:
```scala
val b = a.groupBy(_._1).size
```
notice the _ shorthand used here.

## Join
Pig:
```
C = JOIN A BY $0, B BY $0
```
Scalding:
assuming a and b are both a Pipe[(K,V)], you can join them as follows
```scala
val c = a.join(b)
```

# Scala cheat sheet:
It is recommended to know the basics of Scala when trying out Scalding.
Here are some common things Scala noobs may become confused about coming from Java and Pig.

## Primitive types:
Scala uses the java primitive type names but with the first letter capitalized. (Scala uses the boxed type automatically when needed.)
ex:
Java:
```
final int a = 1
```
Scala:
```scala
val a = 1  // (val means it's a constant. Type is inferred. use var for variables)
val a: Int = 1 // same thing with explicit type declaration
```

## Functions
```
def f(x:Int) = x * 2 // return type inferred

def f(x:Int): Int = x * 2 // same thing with explicit return type
```

## Common types:

### case classes
Immutable data class that can be used in pattern matching
ex:
```scala
case class User(val firstname: String, val lastname: String)
```scala
kind of similar to the following in Java plus the added benefit of pattern matching:
```scala
final class User {
   public final String firstname; // those are immutable so it's fine to make them public
   public final String lastname;
   public User(String firstname,String lastname) {
     this.firstname = firstname;
     this.lastname = lastname;
  }
}
```

### Tuples
fixed size with type assigned to each field
ex:
```scala
val t = (1, "foo") // the type of t is Tuple2[Int, String]
t._1 // => 1
t._2 // => "foo"
```
assigning the members of t to a and b:
```scala
val (a, b) = t
a // => 1
b // => 2
```
it is the same as:
```scala
val a = t._1
val b = t._2
```
it is *not* the same as:
```scala
val a, b = t
// which is:
val a = t
val b = t
```

## pattern matching:
example:

```scala
t match {
  case (a, b) => a
}
```

Which translate to: if t is a `Tuple2`, assign t._1 to a and t._2 to b and return a
You don't need to name things you don't use. The `_` wildcard can be used:

```scala
t match {
  case (a, _) => a
}
```

similarly with case classes:

```scala
val u = User("Jack", "Jackson") // This is the same as User.apply("Jack", "Jackson"). Not a constructor call
var v = u match {
  case User(firstname, lastname) => firstname
  ... // other cases
}
```

More advanced pattern matching

```scala
case class Name(first: String, middle: String, last: String)
case class Address(street: String, zip: String, city: String)
case class Person(name: Name, age: Int, address: Address)
val p = Person(Name("Bob", "E.", "Roberts"), 42, Address("23 colorado st.", "99999", "Las Vegas"))
// unwrap Person
p match  { case Person(a,b,c) => (a,b,c) }
// unwrap Person and Name
p match  { case Person(Name(f,m,l), b, c) => (f, m, l, b, c) }
// multiple case statements (anonymizing minors not in the "Roberts" familly)
p match  {
  case Person(Name(first, _,"Roberts"), _, _) => first // matches only when lastname in Name is "Roberts"
  case Person(Name(first, _, _), age, _) if (age > 21) => first // predicate can be applied as well
  case _ => "anonymous" // default case if none of the above applies
}
// just extracting age
p match  { case Person(_, age, _) => age }
// this one could be
p.age
// flattening the entire structure
p match  { case Person(Name(f,m,l), age, Address(street, zip, city)) => (f, m, l, age, street, zip, city) }
```

## Typed pipes basics

### map
If we have the following
p1 of typed `Pipe[T]`
f of type `Function1[T,U]`
then
```scala
val p2 = p1.map(f)
```
p2 is of type `Pipe[U]`

### lambda syntax

When defining a function inline we use the following syntax:

```scala
(param1, param2, ...) => /* expression */
```
which can be used in map
```scala
p.map( (a) => a + 1 )
```
Here we are defining a function that takes one parameter named `a` and apply it to all elements of p

### map variations

with p1 of type `Pipe[(Int, String)]` (a Pipe of Tuple2[Int, String])
mapping elements in p1:

```scala
p1.map( (t) => t._1 )
```

When a function takes only one parameter *and* is extremely simple, we can use the following shorthand:

```scala
p1.map( _._1 )
```

This syntax defines a function that takes one parameter on which we call \.\_1 (get the first element of the tuple)

WARNING: _ expands only to the expression directly around it. \_.\_1.\_2 works but (\_.\_1).\_2 does not. (it turns into ((t) => (t.\_1)).\_2 which does not compile)
always fallback to the full syntax when in doubt: (t) => (t.\_1).\_2 works

In Scala the syntax for getting a field is the same as for calling a parameter-less method (parens are omitted). In fact getting a fields is calling a parameter-less methods.

### operator notation to call a function:

```scala
 p1 map f
```

is the same as

```scala
 p1.map(f)
```

In Scala every method can be used as an operator. In fact, this is how operators are implemented as symbols are allowed in method names.

```scala
p1 filter { _._1 == 0 } map { _._2 }
```

also:

```scala
p1.map { (t) => t._1 }
```

notice the curly braces, we're executing a block of code that returns a function. the result (last statement) of { } is passed to map

```scala
p1.map { println("foo"); (t) => t._1 }
```

"foo" is printed once (before passing the function to map)

```scala
p1.map { println("foo"); _._1 }
```

"foo" is printed once (before passing the function to map)

```scala
l.map{ (t) => { println("foo"); t._1 } }
```

"foo" is printed for each element

### pattern matching short hand

```scala
p1.map { case (a,b) => a }
```

block of code that returns a partial function that is then passed to map
is a short hand for:

```scala
p1.map( (t) => t match { case (a,b) => a } )
```
