
rec Credentials
   login  String
   pass  String

rec UserInfo - TODO: Any, Equatable, Hashable
   age Int
   photoURLs  [String]?
   creds  Credentials?
   services  { String : [String] }
   name  String?

met user.login UserInfo login
   creds Credentials

met register Bool
    username String
    password String

met ping Void