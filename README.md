
# Burning Bar 📏🔥[![Join the chat at https://gitter.im/cfr/burningbar][badge-gitter]][gitter] [![Unlicensed][badge-license]][license] [![Build Status][badge-travis]][travis] [![Coverage Status](https://coveralls.io/repos/cfr/burningbar/badge.svg?branch=master)](https://coveralls.io/r/cfr/burningbar?branch=master)

<!-- [![Available on Hackage][badge-hackage]][hackage]
     [![Gratipay](http://img.shields.io/gratipay/cfr.svg)][gratipay]
     [![Stories in Ready](https://badge.waffle.io/cfr/burningbar.png?label=ready&title=Ready)](https://waffle.io/cfr/burningbar) -->

Swift RPC generator.

Available [online](http://cfr.pw/burnbar) with simple spec editor and output viewer.

Clone and run:

    $ git clone https://github.com/cfr/burningbar.git
    $ cd burningbar
    $ make example

See [INSTALL](INSTALL.md) for general installation and Xcode integration instruction.

## Usage

Command

    $ burningbar --spec-file spec.burnbar

reads [spec.burnbar][Spec] and generates [Interface.swift][] (RPC funcs) and
[Entities.swift][] (structs/mappers), Interface class is parameterized by
`Transport` protocol containing `call`, `cast`, `listen`, `cancel` funcs and
`CancellationToken` typealias. Struct's `init?(json:)` takes `[String: AnyObject]`
representing JSON object, `json` returns this object back.

See help for more options:

    $ burningbar --help
    burningbar: Usage: burningbar [-vhtirdsbpc]
    http://j.mp/burnbar v0.5.31
      -v                  --version                         print version number
      -h                  --help                            print help
      -t Transport        --transport=Transport             transport protocol name
      -i Iterface         --interface=Iterface              interface class name
      -r Interface.swift  --interface-file=Interface.swift  interface out filename
      -d Entities.swift   --entities-file=Entities.swift    entities out filename
      -s spec.burnbar     --spec-file=spec.burnbar          input spec file
      -b                  --dynamicity-shield               accept weak-typed json
      -p .                --path=.                          output path prefix
      -c                  --validate                        validate spec and exit

## [Spec syntax][Spec]

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

