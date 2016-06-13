# concurrent-map
This library implements a concurrent map in Haskell using skiplists.

## Installation
Install with cabal:
```
cabal install --only-dependencies
cabal configure
cabal install
```
Import ConcurrentMap to integrate in an external program.
See src/ConcurrentMap.hs for further documentation.

## Benchmarks
Build the benchmark-suite:
```
cabal configure --flags=Testing
cabal build benchmark-skiplist
```
Execute './benchmarks.sh' to run the entire benchmark-suite.
The results for each test are written to the folder 'benchmarks'
and can be plotted with gnuplot.
The individual tests are described in my bachelor thesis.
