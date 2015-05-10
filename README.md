# Burning Bar

Swift RPC generator written in Haskell.

Clone and run:

    $ git clone https://github.com/cfr/burningbar.git
    $ cd burningbar
    $ make test

## Playground

See Swift example in Xcode workspace: `$ open TestGen.xcworkspace`.
Build TestGen framework to run generator and compile module for the playground.

## Usage

    $ burningbar --spec-file spec.burbar

reads [spec.burbar][Spec] and generates [Interface.swift][], [Entities.swift][] with
corresponding structs/parsers and RPC funcs declarations. Functions are encapsulated
in Interface class, parameterized by `Transport` protocol containing `call` func
and `CancellationToken` typealias. Structs `init(json:)` takes `[String: AnyObject]`
representing json object.

See help for more options:

    $ burningbar --help
    burningbar: Usage: burningbar [-vhtirdsp]
    http://j.mp/burningbar v0.5.10
    -v                  --version                         print version number
    -h                  --help                            print help
    -t Transport        --transport=Transport             transport protocol name
    -i Iterface         --interface=Iterface              interface class name
    -r Interface.swift  --interface-file=Interface.swift  interface out filename
    -d Entities.swift   --entities-file=Entities.swift    entities out filename
    -s spec.burbar      --spec-file=spec.burbar           input spec file
    -p .                --path=.                          output path prefix

   [Interface.swift]: TestGen/Interface.swift
   [Entities.swift]: TestGen/Entities.swift
   [Spec]: spec.burbar

## Spec syntax

    - comments starts with dash
    - methods defined with met
    - local (pretty) name is optional
    - see type and return type grammar below
    met remoteName returnType [name]
     name type - first arg
     ... - more arguments
    
    - record
    rec name
     name varType - non-optional is not implemented yet
     ...

where

    type → array-type | dictionary-type | type-identifier | optional-type | primitive-type
    varType → optional-type
    returnType → type-identifier
    primitive-type → String | Bool | Int | Float | NSNumber
    array-type → [ type ]
    dictionary-type → [ type : type ]
    optional-type → type ?
    implicitly-unwrapped-optional-type → type !

