## Set Up

Suggested workflow: Everyone creates his personal repository, cloned from the main "fpinscala" project. Everyone pushes the exercises to her personal repository only. This way all repositories are visible in one place, and everyone can look into the commits made by others and comment. Common changes like additional scala tests can be pushed to the main repository and everyone can pull them into his personal repo.

Here are the steps to setup everything.

Create the personal repository, go to https://github.com/fpinscala-muc and create a new repository called "pfinscala-<YOUR-GITHUB-USERNAME>", select "public" and don't initialize the repo yet.

Clone the "main" respsitory:

    $ git clone git@github.com:fpinscala-muc/fpinscala.git
    $ cd fpinscala

If you use your work notebook you may not want to use your real name and TD email address for commits as they will be visible in public, in that case configure your username and email for the repository only. (Hint: [Keeping your email address private](Keeping your email address private)):

    $ git config user.email "youremail@example.com"
    $ git config user.name "Your name or pseudonym"

Configure and initialize your personal repository:

    $ git remote remove origin
    $ git remote add origin git@github.com:fpinscala-muc/fpinscala-<YOUR-GITHUB-USERNAME>.git
    $ git remote add upstream git@github.com:fpinscala-muc/fpinscala.git
    $ git push -u origin master

### Get updates from main repo

When changes are made to the main `git@github.com:fpinscala-muc/fpinscala.git` repository (e.g. new tests) those changes can be pulled by `$ git pull upstream master`.

### Import projects into Eclipse

To import the project into Eclipse the project files have to be generated first.

    $ sbt eclipse

If not done yet the sbteclipse plugin has to be configured, add the following to `~/.sbt/0.13/plugins/plugins.sbt`:

    addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "2.5.0")

See also https://github.com/typesafehub/sbteclipse/

## Chapter 1: What is functional programming?
### Additional links
* http://www.youtube.com/watch?v=1gZAqJA2pEk This video by Paul Chiusano is about an earlier version (with a different example) of the first chapter. It gives a nice intro & summary what the book is up to.
* http://www.youtube.com/watch?v=-FRm3VPhseI&index=2&list=PL693EFD059797C21E The Clean Code Talk with the credit card example (~ 19 min) where he had to use his own credit card # for testing and had the money indeed debited J I think this inspired the example in the book. For the other videos just search for “Clean Code Talk on youtube).
* http://steve-yegge.blogspot.de/2006/03/execution-in-kingdom-of-nouns.html The Kingdom of Nouns
* http://www.manning.com/suereth/Suereth_MEAP_CH01.pdf First chapter of “Scala in Depth”, discussing FP vs. OOP
* http://james-iry.blogspot.de/2007/08/kingdom-of-nerbs.html

## Chapter 2: Getting started with functional programming in Scala
### Notes
#### “return” keyword in Scala
Actually, I have never ever used it, so I had to look up the exact meaning.
Seems to be similar to Java, but try to avoid it. Really! There might be only a few useful cases:
http://stackoverflow.com/questions/3770989/purpose-of-return-statement-in-scala
 
#### Is the book’s partial1 function really a case of a closure?
According to Wikipedia it is: http://en.wikipedia.org/wiki/Closure_%28computer_science%29
The function incrementBy in the first example is a closure b/c of variable x, defined outside its body/lexical scope.
This is exactly like the returned function from partial1.

## Chapter 3: Functional data structures
### Notes
#### type Nothing
Quote from http://programmers.stackexchange.com/questions/195793/how-is-nothing-a-subtype-of-every-other-type-in-scala :
"As for how this is done: We don't know, it's compiler magic and an implementation detail.
Quite often a language does things you as a programmer can't. As a counterpart to Nothing: Everything in Scala inherits from Any, everything except Any. Why doesn't Any inherit from something? You can't do that. Why can Scala do that? Well, because Scala set the rules, not you. Nothing being a subtype of everything is just an other instance of this."
And here is what Big M has to say about the Scala type hierarchy in general: http://www.artima.com/pins1ed/scalas-hierarchy.html

#### Currying, partional and higher order functions
* http://danielwestheide.com/blog/2013/01/30/the-neophytes-guide-to-scala-part-11-currying-and-partially-applied-functions.html
* http://danielwestheide.com/blog/2013/01/23/the-neophytes-guide-to-scala-part-10-staying-dry-with-higher-order-functions.html

### Additional links
For the easily bored:
* http://www.codecommit.com/blog/scala/scala-collections-for-the-easily-bored-part-1 Scala Collections FTEB
* http://www.codecommit.com/blog/scala/scala-collections-for-the-easily-bored-part-2
* http://www.codecommit.com/blog/scala/scala-collections-for-the-easily-bored-part-3
* http://danielwestheide.com/blog/2012/12/05/the-neophytes-guide-to-scala-part-3-patterns-everywhere.html Pattern Matching
* http://danielwestheide.com/blog/2012/12/05/the-neophytes-guide-to-scala-part-3-patterns-everywhere.html
* http://typelevel.org/blog/2013/10/13/towards-scalaz-1.html Addition FTEB
* http://typelevel.org/blog/2013/12/15/towards-scalaz-2.html Fold FTEB

### Chapter 4: Handling errors without exceptions
### Notes
#### variance implemented with for-comprehension
First here's a straightforward solution using flatMap & map:  

    def variance(xs: Seq[Double]): Option[Double] =
      mean(xs) flatMap { m =>
        mean(xs map(x => math.pow(m - x, 2)))
      }

Jörg could not fall asleep after today's session before he had not solved the variance w/ for-comprehension puzzle! So here is his solution:  

    def variance(xs: Seq[Double]): Option[Double] =
      for {
        m <- mean(xs)
        v <- mean(xs map (x => pow(x - m, 2)))
      } yield v

This arguably looks and read better than the flatMap/map combinator solution, and by all means feel free to write code like this in for-comprehension style!
Yesterday I (Achim) just saw the pattern "flatMap followed by map" and immediately read that as just one for-comprehension.
I was wrong here b/c the flatMap belongs to an Option (the result of "mean(xs)") whereas map belongs to a Seq (namely "xs").
So if you don't want to go for nested for-comprehensions Jörg's solution is the best you can get here. Which is more than just acceptable, I would say.
Thanks, Jörg! And I hope you had sweet dreams after this.

#### scala.util.Try
See the Additional links below for some (of many) tutorials for Try.
When it comes to FP error handling in concrete daily life cases I personally ask myself the following questions:

* Do I care about the error at all? If not use Option.
* Do I care about the error, and I want to break up at the first error and pass it along to the caller? Use Try.
* Do I need to accumulate errors and pass them along to the caller? Use something fancy like scalaz.Validation.

The article "How do I error handle thee?" (the typelevel.org link below) gives a nice summary.  
But if you are a purist then Try is out of the question - it is not a true Monad!

#### Paul Phillips, disappointed lover
Here he stays relatively calm: http://www.youtube.com/watch?v=uiJycy6dFSQ  
And here he is running wild: http://www.youtube.com/watch?v=TS1lpKBMkgg  
But some discussions following his rants:  
https://groups.google.com/forum/#!topic/scala-user/ImqlClXTrS4%5B1-25-false%5D  
http://www.linkedin.com/groups/Can-anyone-watch-Paul-Phillips-746917.S.5837580490751639556  
https://news.ycombinator.com/item?id=7258866  
MHO: Scala collections seem to work as expected (even just great if compare them to Java collections or helper libs like Guava),
so why bother too much about internals?

### Additional links
* http://danielwestheide.com/blog/2012/12/19/the-neophytes-guide-to-scala-part-5-the-option-type.html
* http://danielwestheide.com/blog/2012/12/26/the-neophytes-guide-to-scala-part-6-error-handling-with-try.html Try has its theoretical flaws, but in practice ...
* http://danielwestheide.com/blog/2013/01/02/the-neophytes-guide-to-scala-part-7-the-either-type.html
* http://www.michaelpollmeier.com/scala-util-try-chain-to-handle-all-success-and-error-cases/
* http://ymasory.github.io/error-handling-in-scala
* http://typelevel.org/blog/2014/02/21/error-handling.html
* http://aboutwhichmorelater.tumblr.com/post/30409572482/scala-util-try
* https://gist.github.com/ms-tg/6222775

### Chapter 5: Strictness and laziness

### Additional links
* http://www.nurkiewicz.com/2013/05/lazy-sequences-in-scala-and-clojure.html
* https://github.com/ardlema/scala-lazy-evaluation (exercise from Coursera "Principles of Reactive Programming" course https://www.coursera.org/course/reactive)


