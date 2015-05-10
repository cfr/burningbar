# Burning Bar

Swift RPC generator written in Haskell.

Install and run:

    $ git clone https://github.com/cfr/burningbar.git
    $ cd burningbar
    $ make test

See Swift example in Xcode workspace: `$ open TestGen.xcworkspace`.

Build TestGen framework to run generator and compile module for the playground.


    $ burningbar --spec-file spec.bb

reads [spec.bb][Spec] and generates [Interface.swift][], [Entities.swift][] with
corresponding structs/parsers and RPC funcs declarations. Functions are encapsulated
in Interface class, parameterized by `Transport` protocol containing `call` func
and `CancellationToken` typealias. Structs `init(json:)` takes `[String: AnyObject]`
representing json object.

See help for more options:

    $ burningbar --help

   [Interface.swift]: [https://github.com/cfr/burningbar/blob/master/TestGen/Interface.swift]
   [Entities.swift]: [https://github.com/cfr/burningbar/blob/master/TestGen/Entities.swift]
   [Spec]: [https://github.com/cfr/burningbar/blob/master/spec.bb]

Spec syntax:

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

