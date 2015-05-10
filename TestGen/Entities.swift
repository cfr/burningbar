// 📏🔥 Generated with http://j.mp/burnbar v0.5.10

import Foundation

public struct Credentials {
    public init(_ json: [String: AnyObject]) {
        login = json["login"] as? String
        pass = json["pass"] as? String
    }
    public var serialized: [String: AnyObject] { get {
        return ["login": login?.serialized ?? "null" as AnyObject, "pass": pass?.serialized ?? "null" as AnyObject] } }
    public var login: String?
    public var pass: String?
}

public struct UserInfo {
    public init(_ json: [String: AnyObject]) {
        friends = [String: UserInfo]()
        age = json["age"] as? NSNumber
        photoURLs = json["photoURLs"] as? [String]
        if let json = json["creds"] as? [String: AnyObject] {
          creds = Credentials(json)
        } else { creds = nil }
        map(json.keys) {(k: String) in self.friends[k] = UserInfo(json[k] as! [String: AnyObject])}
        name = json["name"] as? String
    }
    public var serialized: [String: AnyObject] { get {
        return ["age": age?.serialized ?? "null" as AnyObject, "photoURLs": photoURLs?.serialized ?? "null" as AnyObject, "creds": creds?.serialized ?? "null" as AnyObject, "friends": friends.serialized as AnyObject, "name": name?.serialized ?? "null" as AnyObject] } }
    public var age: NSNumber?
    public var photoURLs: [String]?
    public var creds: Credentials?
    public var friends: [String: UserInfo]
    public var name: String?
}


internal extension Dictionary {
  var serialized: [String: AnyObject] {
    get { var d = [String: AnyObject]()
          for key in self.keys { d[key as! String] = self[key] as? AnyObject }
          return d }
  }
}
internal extension Array {
  var serialized: [AnyObject] {
    get { var a = [AnyObject]()
          for i in self { if let o: AnyObject = i as? AnyObject { a.append(o) } }
          return a }
  }
}
internal extension Int {
  var serialized: String {
    get { return self.description } 
  }
}
internal extension String {
  var serialized: String {
    get { return self } 
  }
}
internal extension NSNumber {
  var serialized: String {
    get { return self.description } 
  }
}
internal extension Bool {
  var serialized: String {
    get { return self ? "true" : "false" } 
  }
}