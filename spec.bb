- comment starts with dash
-
- # Method
-
- defined with met
- name is optional, remoteName is used if missing
- see type and return type grammar below
-
-     met remoteName [as localName][: returnType]
-      name type - first arg
-      ... - more arguments
-
- # Record
-
-     rec remoteName [as localName][: super/protos]
-      name varType [ = defaultValue] - same as arg, optional default value
-      ...
-
- `Printable`/`Equatable` in protos adds `description`/(`==`) to struct.
-
- # Type grammar
-
- type → array-type | dictionary-type | type-identifier | optional-type | primitive-type
- returnType → type-identifier | Void
- primitive-type → String | NSNumber | overloaded
- overloaded → Int | Float | Bool | NSDate # enabled with --overloaded flag
- array-type → [ type ]
- dictionary-type → { String : type }
- optional-type → type ?
-
- # Examples:
-

met ping

rec creds as Credentials: Equatable, YourProto
   login String = "user"
   pass String

rec User: YourProto, Printable, Equatable
    age Int?
    photoURLs [String]?
    creds Credentials?
    friends { String : User }
    name String
    birth NSDate?

met user.login as login: Void
   creds Credentials

met register: Credentials
    username String
    password String

