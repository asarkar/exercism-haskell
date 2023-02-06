# exercism-haskell
My solutions for [Exercism Haskell Track](https://exercism.org/tracks/haskell).
Feel free to open issues for questions, comments, or suggestions.

[![](https://github.com/asarkar/exercism-haskell/workflows/CI/badge.svg)](https://github.com/asarkar/exercism-haskell/actions)

For detailed explanations of solutions, see [exercism-rust](https://github.com/asarkar/exercism-rust).

A curated list of [Haskell resources](https://blogs.asarkar.com/haskell-curated/) is available on my blog.

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

To use a language extension
```
GHCi> :set -XOverloadedStrings
```

To show type of something
```
GHCi> :t variable
```

Multiline input
```
GHCi> :{
	...
:}
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

To run exactly matching tests:
```
stack test --ta '-m "/<some_word>/"'
```

To run a _specific test_:
```
stack test --ta '-m "/allergies/no allergies at all/"'
```

To view package list:
```
stack ls dependencies
```

## License

Released under [Apache License v2.0](LICENSE).
