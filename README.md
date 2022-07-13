# exercism-rust
My solutions for [Exercism Haskell Track](https://exercism.org/tracks/haskell).
Feel free to open issues for questions, comments, or suggestions.

[![](https://github.com/asarkar/exercism-haskell/workflows/CI/badge.svg)](https://github.com/asarkar/exercism-haskell/actions)

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
stack --work-dir . --stack-yaml </path/to/stack.yaml> test
```

To run all matching tests:
```
stack --work-dir . --stack-yaml </path/to/stack.yaml> test --ta '-m <some_word>'
```

To run a single test:
```
stack --work-dir . --stack-yaml </path/to/stack.yaml> test --ta '-m "/<test_name>/"'
```


## References

### Tools

* [The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/GUIDE/)
* [GHC Warnings](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/using-warnings.html)
* [Structuring your first Haskell project with Stack](https://sakshamsharma.com/2018/03/haskell-proj-struct/)
* [Ormolu](https://ormolu-live.tweag.io/)
* [pointfree.io](http://pointfree.io/)

### Control Structures

* [case](https://wiki.haskell.org/Case)
* [Control structures](https://en.wikibooks.org/wiki/Haskell/Control_structures)
* [Let vs. Where](https://wiki.haskell.org/Let_vs._Where)
* [case expression in a do block](https://stackoverflow.com/a/156050/839733)
* [Some deep nested indentation](https://stackoverflow.com/q/47622491/839733)
* [let with guards](https://stackoverflow.com/a/46888423/839733)
* [case within let indentation](https://stackoverflow.com/a/33010779/839733)
* [Guard within case](https://www.reddit.com/r/haskell/comments/bkn97b/use_variable_inside_case_of_in_haskell/)

### General

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

### Monad

* [The State Monad: A Tutorial for the Confused?](http://brandon.si/code/the-state-monad-a-tutorial-for-the-confused/)
* [The State Monad](https://acm.wustl.edu/functional/state-monad.php)
* [State Monad](https://mmhaskell.com/monads/state)
* [State Monad Comes To Help Sequential Pattern Matching](https://kowainik.github.io/posts/2018-11-18-state-pattern-matching)

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


### Haskell Projects/Challenges/Puzzles

* [Haskell Projects](https://acm.wustl.edu/functional/projects.php)
* [Ninety-Nine Haskell Problems](https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems)

## License

Released under [Apache License v2.0](LICENSE).