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

## Formatting

https://monadfix.com/ormolu

## References

* [The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/GUIDE/)
* [GHC Warnings](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/using-warnings.html)
* [4 Steps to a Better Imports List in Haskell](https://hackernoon.com/4-steps-to-a-better-imports-list-in-haskell-43a3d868273c)
* [Structuring your first Haskell project with Stack](https://sakshamsharma.com/2018/03/haskell-proj-struct/)
* [Functional Programming in Haskell](https://www.youtube.com/playlist?list=PLF1Z-APd9zK7usPMx3LGMZEHrECUGodd3)
* [Advanced Functional Programming in Haskell](https://www.youtube.com/playlist?list=PLF1Z-APd9zK5uFc8FKr_di9bfsYv8-lbc)
* [Kowainik Blog](https://kowainik.github.io/tags/haskell)
* [List comprehension](https://wiki.haskell.org/List_comprehension)
* [Haskell syntax for a case expression in a do block](https://stackoverflow.com/a/156050/839733)
* [The State Monad: A Tutorial for the Confused?](http://brandon.si/code/the-state-monad-a-tutorial-for-the-confused/)
* [The State Monad](https://acm.wustl.edu/functional/state-monad.php)
* [State Monad](https://mmhaskell.com/monads/state)
* [State Monad Comes To Help Sequential Pattern Matching](https://kowainik.github.io/posts/2018-11-18-state-pattern-matching)
* [GADTs for dummies](https://wiki.haskell.org/GADTs_for_dummies)
* [Haskell Projects](https://acm.wustl.edu/functional/projects.php)

## License

Released under [Apache License v2.0](LICENSE).