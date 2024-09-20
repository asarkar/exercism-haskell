# exercism-haskell
My solutions for [Exercism Haskell Track](https://exercism.org/tracks/haskell).
Feel free to open issues for questions, comments, or suggestions.

[![](https://github.com/asarkar/exercism-haskell/workflows/CI/badge.svg)](https://github.com/asarkar/exercism-haskell/actions)

A curated list of [Haskell resources](https://blogs.asarkar.com/technical/haskell-curated/) is available on my blog.

## Installation

https://stackoverflow.com/a/78961866/839733

## Uninstallation
```
ghcup nuke
rm -rf $HOME/.ghcup
rm -rf $HOME/.stack
```

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

To load a file
```
GHCi> :load path/to/file
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

All of the `stack test` commands must have the following options appended at the end:
```
--stack-yaml <project>/stack.yaml --resolver lts --verbosity warn --work-dir .
```

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

The name of a failing test is printed on the console, we can copy-paste from there.

To view package list:
```
stack ls dependencies
```

## License

Released under [Apache License v2.0](LICENSE).
