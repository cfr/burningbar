# Burning Bar

[![Join the chat at https://gitter.im/cfr/burningbar](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/cfr/burningbar?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Unlicensed][badge-license]][license]

Swift RPC generator written in Haskell.

Clone and run:

    $ git clone https://github.com/cfr/burningbar.git
    $ cd burningbar
    $ make test

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

   [Interface.swift]: TestGen/Interface.swift
   [Entities.swift]: TestGen/Entities.swift
   [Spec]: spec.burnbar
   [license]: UNLICENSE
   [badge-license]: https://img.shields.io/badge/license-Unlicense-brightgreen.svg

