# Burning Bar [![Join the chat at https://gitter.im/cfr/burningbar][badge-gitter]][gitter] [![Unlicensed][badge-license]][license] [![Build Status][badge-travis]][travis] [![Coverage Status](https://coveralls.io/repos/cfr/burningbar/badge.svg)](https://coveralls.io/r/cfr/burningbar)

<!-- [![Available on Hackage][badge-hackage]][hackage] [![Gratipay](http://img.shields.io/gratipay/cfr.svg)][gratipay] -->

Swift RPC generator written in Haskell.

Clone and run:

    $ git clone https://github.com/cfr/burningbar.git
    $ cd burningbar
    $ make example

## Playground

See Swift example in Xcode workspace: `$ open TestGen.xcworkspace`.
Build TestGen framework to run generator and compile module for the playground.

## Usage

    $ burningbar --spec-file spec.burnbar

reads [spec.burnbar][Spec] and generates [Interface.swift][], [Entities.swift][] with
corresponding structs/parsers and RPC funcs declarations. Functions are encapsulated
in Interface class, parameterized by `Transport` protocol containing `call` func
and `CancellationToken` typealias. Struct's `init(json:)` takes `[String: AnyObject]`
representing json object.

See help for more options:

    $ burningbar --help
    burningbar: Usage: burningbar [-vhtirdsp]
    http://j.mp/burnbar v0.5.10
    -v                  --version                         print version number
    -h                  --help                            print help
    -t Transport        --transport=Transport             transport protocol name
    -i Iterface         --interface=Iterface              interface class name
    -r Interface.swift  --interface-file=Interface.swift  interface out filename
    -d Entities.swift   --entities-file=Entities.swift    entities out filename
    -s spec.burnbar     --spec-file=spec.burnbar          input spec file
    -p .                --path=.                          output path prefix

## [Spec syntax][Spec]

## Service

Generator available [online](http://cfr.pw/burnbar), with a simple [HTTP server](Service.hs)
and a [editor interface](https://github.com/cfr/cfr.github.io/blob/master/burnbar.html).

   [Interface.swift]: TestGen/Interface.swift
   [Entities.swift]: TestGen/Entities.swift
   [Spec]: spec.burnbar
   [license]: UNLICENSE
   [badge-license]: https://img.shields.io/badge/license-Unlicense-brightgreen.svg
   [badge-cabal]: https://wiki.haskell.org/wikiupload/4/43/Built-with-Cabal-light.png
   [cabal]: https://www.haskell.org/cabal
   [badge-gitter]: https://img.shields.io/badge/gitter-join%20chat-brightgreen.svg
   [gitter]: https://gitter.im/cfr/burningbar?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge
   [badge-travis]: https://travis-ci.org/cfr/burningbar.svg?branch=master
   [travis]: https://travis-ci.org/cfr/burningbar
   [badge-haskell]: http://evenmere.org/~bts/haskell-logo/logo-0.svg
   [haskell]: https://haskell.org
   [badge-gratipay]: http://img.shields.io/gratipay/cfr.svg
   [gratipay]: https://www.gratipay.com/cfr
   [badge-hackage]: https://img.shields.io/hackage/v/burningbar.svg?dummy
   [hackage]: https://hackage.haskell.org/package/burningbar
   [badge-coverage]: https://coveralls.io/repos/cfr/burningbar/badge.svg
   [coverage]: https://coveralls.io/r/cfr/burningbar



