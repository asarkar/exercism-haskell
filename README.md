# exercism-rust
[Exercism Haskell Track](https://exercism.org/tracks/haskell)

[![](https://github.com/asarkar/exercism-haskell/workflows/CI/badge.svg)](https://github.com/asarkar/exercism-haskell/actions)

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
* [4 Steps to a Better Imports List in Haskell](https://hackernoon.com/4-steps-to-a-better-imports-list-in-haskell-43a3d868273c)
* [Structuring your first Haskell project with Stack](https://sakshamsharma.com/2018/03/haskell-proj-struct/)
* [Functional Programming in Haskell](https://www.youtube.com/playlist?list=PLF1Z-APd9zK7usPMx3LGMZEHrECUGodd3)
* [Advanced Functional Programming in Haskell](https://www.youtube.com/playlist?list=PLF1Z-APd9zK5uFc8FKr_di9bfsYv8-lbc)