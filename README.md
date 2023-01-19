# exercism-haskell
My solutions for [Exercism Haskell Track](https://exercism.org/tracks/haskell).
Feel free to open issues for questions, comments, or suggestions.

[![](https://github.com/asarkar/exercism-haskell/workflows/CI/badge.svg)](https://github.com/asarkar/exercism-haskell/actions)

For detailed explanations of solutions, see [exercism-rust](https://github.com/asarkar/exercism-rust).

## GHCi Commands
To start with a package loaded
```
stack repl --package xyz
```

To load if already started
```
GHCi> :set -package xyz
GHCi> :m +XYZ.Module.You.Suddenly.Need
```

To show type of something
```
GHCi> :t variable
```

To quit
```
GHCi> :quit
```


## Running tests

```
stack test
```

To run all matching tests:
```
stack test --ta '-m <some_word>'
```

To run a single test:
```
stack test --ta '-m "/<test_name>/"'
```

To view package list:
```
stack ls dependencies
```

## References

* https://www.haskell.org/documentation/

### Tools

* [The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/GUIDE/)
* [GHC Warnings](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/using-warnings.html)
* [Structuring your first Haskell project with Stack](https://sakshamsharma.com/2018/03/haskell-proj-struct/)
* [Ormolu](https://ormolu-live.tweag.io/)
* [pointfree.io](http://pointfree.io/)

### Control Structures

* [case](https://wiki.haskell.org/Case)
* [Haskell/Control structures](https://en.wikibooks.org/wiki/Haskell/Control_structures)
* [Let vs. Where](https://wiki.haskell.org/Let_vs._Where)
* [case expression in a do block](https://stackoverflow.com/a/156050/839733)
* [Some deep nested indentation](https://stackoverflow.com/q/47622491/839733)
* [let with guards](https://stackoverflow.com/a/46888423/839733)
* [case within let indentation](https://stackoverflow.com/a/33010779/839733)
* [Guard within case](https://www.reddit.com/r/haskell/comments/bkn97b/use_variable_inside_case_of_in_haskell/)


### General

* [graninas/software-design-in-haskell](https://github.com/graninas/software-design-in-haskell)
* [Haskell Cheat Sheet](http://cheatsheet.codeslower.com/CheatSheet.pdf)
* [4 Steps to a Better Imports List in Haskell](https://hackernoon.com/4-steps-to-a-better-imports-list-in-haskell-43a3d868273c)
* [Functional Programming in Haskell](https://www.youtube.com/playlist?list=PLF1Z-APd9zK7usPMx3LGMZEHrECUGodd3)
* [Advanced Functional Programming in Haskell](https://www.youtube.com/playlist?list=PLF1Z-APd9zK5uFc8FKr_di9bfsYv8-lbc)
* [Kowainik Blog](https://kowainik.github.io/tags/haskell)
* [Pattern Matching](https://kowainik.github.io/posts/2018-11-18-state-pattern-matching)
* [GADTs for dummies](https://wiki.haskell.org/GADTs_for_dummies)
* [Learn Haskell](https://markkarpov.com/learn-haskell.html)
* [Smart Constructors](https://wiki.haskell.org/Smart_constructors)
* [List comprehension](https://wiki.haskell.org/List_comprehension)
* [Serokell Blog](https://serokell.io/blog)
* [DevTut](https://devtut.github.io/haskell/)
* [Random Hacks](http://www.randomhacks.net/)
* [Roman Cheplyaka Blog](https://ro-che.info/articles/haskell)
* [FPComplete](https://www.fpcomplete.com/haskell/learn/)
* [Artyom's tech notes](https://tek.brick.do/)
* [Monadfix blog](https://blog.monadfix.com/)
* [PatternSynonyms](https://riptutorial.com/haskell/example/16064/patternsynonyms)
* [View Patterns in GHC](https://dlicata.wescreates.wesleyan.edu/pubs/lpj07views/lpj07views-anglohaskell.pdf)
* [Lists vs Vectors vs Sequences](https://stackoverflow.com/a/9613203/839733)
* [Donnacha Oisin Kidney](https://doisinkidney.com/)
* [Carlo Hamalainen](https://carlo-hamalainen.net/)
* [Advent of Haskell](https://medium.com/@mvaldesdeleon/advent-of-haskell-950d6408a729)
* [Mutable State in Haskell](https://blog.jakuba.net/2014-07-20-mutable-state-in-haskell/)
* [The vector package](https://www.schoolofhaskell.com/user/commercial/content/vector#boxed--storable-and-unboxed)
* [Fast Sudoku Solver in Haskell](https://abhinavsarkar.net/posts/fast-sudoku-solver-in-haskell-1/)

### Monad

* [The State Monad: A Tutorial for the Confused?](http://brandon.si/code/the-state-monad-a-tutorial-for-the-confused/)
* [The State Monad](https://acm.wustl.edu/functional/state-monad.php)
* [State Monad](https://mmhaskell.com/monads/state)
* [State Monad Comes To Help Sequential Pattern Matching](https://kowainik.github.io/posts/2018-11-18-state-pattern-matching)
* [Note to self: reader monad transformer](https://carlo-hamalainen.net/2014/03/05/note-to-self-reader-monad-transformer/)
* [A Simple Reader Monad Example](https://blog.ssanj.net/posts/2014-09-23-A-Simple-Reader-Monad-Example.html)
* [ReaderT WriterT Monad Transformer Stack in Haskell](https://blog.ssanj.net/posts/2018-01-12-stacking-the-readert-writert-monad-transformer-stack-in-haskell.html)
* [Monads in 15 minutes: Backtracking and Maybe](http://www.randomhacks.net/2007/03/12/monads-in-15-minutes/)
* [Haskell/Understanding monads/State](https://en.wikibooks.org/wiki/Haskell/Understanding_monads/State)
* [State Monad](https://wiki.haskell.org/State_Monad)
* [Difference between State, ST, IORef, and MVar](https://stackoverflow.com/q/5545517/839733)
* [Haskell: State and StateT examples](https://cstml.github.io/2021/07/22/State-Monad.html)
* [Simple StateT use](https://wiki.haskell.org/Simple_StateT_use)
* [Adventures in Three Monads](__asset__/threemonads.pdf)
* [Backtracking, Interleaving, and Terminating Monad Transformers](__asset__/LogicT.pdf)

### Megaparsec

* [Monday Morning Haskell](https://mmhaskell.com/parsing/megaparsec)
* [Megaparsec tutorial](https://markkarpov.com/tutorial/megaparsec.html)
* [Parser Combinators in Haskell](https://serokell.io/blog/parser-combinators-in-haskell#megaparsec-tutorial)
* [Beginner's guide to Megaparsec](https://akashagrawal.me/2017/01/19/beginners-guide-to-megaparsec.html)
* [Exploring parser combinators](https://thewagner.net/blog/2019/05/03/exploring-parser-combinators/)
* [Using Megaparsec on the WHILE Language](https://gist.github.com/CMCDragonkai/1d46c0860d6bce857d516a35fab23d52)
* [Megaparsec basics](https://funprog.srid.ca/haskell/megaparsec-basics.html)
* [Megaparsec](https://blog.josephmorag.com/posts/mcc1/#headline-4)
* [Pragmatic Haskell for Beginners, Lecture 1](https://begriffs.com/posts/2016-05-14-pragmatic-haskell-1.html)

### Date/Time

* [A Cheatsheet To The Time Library](https://williamyaoh.com/posts/2019-09-16-time-cheatsheet.html)
* [Quick guide to basic Date/Time operations in Haskell](https://www.bitestring.com/posts/2021-07-10-haskell-datatime-basics.html)
* [A Haskell Time Library Tutorial](https://two-wrongs.com/haskell-time-library-tutorial.html)


### Haskell Projects/Challenges/Puzzles

* [Haskell Projects](https://acm.wustl.edu/functional/projects.php)
* [Ninety-Nine Haskell Problems](https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems)

### Testing

* [QuickCheck: Type-directed Property Testing](https://cseweb.ucsd.edu/classes/wi14/cse230-a/lectures/lec-quickcheck.html)
* [Property Testing using QuickCheck](https://www.dcc.fc.up.pt/~pbv/aulas/tapf/handouts/quickcheck.html)
* [QuickCheck and Magic of Testing](https://www.fpcomplete.com/blog/2017/01/quickcheck/)
* [The Design and Use of QuickCheck](https://begriffs.com/posts/2017-01-14-design-use-quickcheck.html)
* [Write you some QuickCheck](https://blog.nikosbaxevanis.com/2016/02/08/write-you-some-quickcheck/)

## License

Released under [Apache License v2.0](LICENSE).
