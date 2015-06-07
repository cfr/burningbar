# INSTALL

You have three options for building/running generator: make, cabal and runhaskell.
Anyway you have to install [ghc](https://www.haskell.org/ghc/) or compatible Haskell2010 compiler.

Tested on OS X and Ubuntu.

## Building

### [Makefile](makefile)

    $ make

Will try to install dependency (ghc, using [brew.sh](http://brew.sh) or apt) and compile `./xcode/burningbar` executable.
Repo provides OS X binary to use with Xcode.

### [Cabal](burningbar.cabal)

Cabal is part of ghc or [haskell platform](https://www.haskell.org/platform/).

    $ cabal configure
    $ cabal install

Builds `burningbar` and [`burningbard` http service](src/Service.hs) in `dist/build/`.
burningbard depends on dozen of hackage packages and cabal will install them in `~/.cabal`.

Use `burningbard` as http service:

    $ dist/build/burningbard/burningbard &
    [1] 17638
    Info 0: Listening on 0.0.0.0:9604
    $ curl --data @spec.burnbar localhost:9604
    Info 0: Accepted connection from 127.0.0.1:53940
    { "Entities": "import Foundation...

See also [online editor](http://cfr.pw/burnbar) ([src](https://github.com/cfr/cfr.github.io/blob/master/burnbar.html)).

### Interpret

[`runburningbar`](runburningbar) script runs generator in interpreted mode.
Requires ghc(i) (or any other interpreter providing `runhaskell` cmd).

## Xcode integration

Generator integrated as a "Run Script" build phase in example Xcode workspace.

    $ open xcode/TestGen.xcworkspace

Build TestGen framework to run generator and compile generated module.
See example [`Playground`](xcode/TestGen/TestGen.playground/Contents.swift).
Drop `TestGen` project into your workspace to embed generator.

