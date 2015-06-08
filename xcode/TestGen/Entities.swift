// 📏🔥 Generated with http://j.mp/burnbar v0.6.7-α

import Foundation

public struct Credentials: JSONEncodable, JSONDecodable, Equatable, YourProto {
    public let json: [String : AnyObject]
    public let login: String
    public let pass: String
    public let Name: String
    public static let Name = "Credentials"
    public static let json = "json"
    public static let login = "login"
    public static let pass = "pass"
    public init?(json: [String : AnyObject]) {
        let (c, json) = (Credentials.create(json), json)
        if let c_ = (c, json) ~~ "login" ~~ "pass" { self = c_; return }
        return nil
    }
    public init(_ json: [String : AnyObject], login: String, pass: String) {
        self.json = json; self.login = login; self.pass = pass; self.Name = "Credentials"
    }
    static func create(json: [String : AnyObject])(login: String)(pass: String) -> Credentials {
        return Credentials(json, login: login, pass: pass)
    }
}
public func == (lhs: Credentials , rhs: Credentials ) -> Bool { return lhs.json.description == rhs.json.description }

public struct User: JSONEncodable, JSONDecodable, YourProto, Printable, Equatable {
    public let json: [String : AnyObject]
    public let age: Int?
    public let photoURLs: [String]
    public let creds: Credentials?
    public let friends: [String: User]
    public let name: String
    public let Name: String
    public static let Name = "User"
    public static let json = "json"
    public static let age = "age"
    public static let photoURLs = "photoURLs"
    public static let creds = "creds"
    public static let friends = "friends"
    public static let name = "name"
    public init?(json: [String : AnyObject]) {
        let (c, json) = (User.create(json), json)
        if let (c_, json) = (c, json)  ~~? "age" ~~ "photoURLs" ~~? "creds" {
          if let c__ = (c_, json) ~~ "friends" ~~ "name" { self = c__; return }
        }
        return nil
    }
    public init(_ json: [String : AnyObject], age: Int?, photoURLs: [String], creds: Credentials?, friends: [String: User], name: String) {
        self.json = json; self.age = age; self.photoURLs = photoURLs; self.creds = creds; self.friends = friends; self.name = name; self.Name = "User"
    }
    static func create(json: [String : AnyObject])(age: Int?)(photoURLs: [String])(creds: Credentials?)(friends: [String: User])(name: String) -> User {
        return User(json, age: age, photoURLs: photoURLs, creds: creds, friends: friends, name: name)
    }
    public var description: String { return Name + ": " + json.description }
}
public func == (lhs: User , rhs: User ) -> Bool { return lhs.json.description == rhs.json.description }

public protocol JSONEncodable {
    var json: [String : AnyObject] { get }
    var Name: String { get }
}
public protocol JSONDecodable {
    init?(json: [String : AnyObject])
}

// JSON mapping operators

typealias JSON = [String : AnyObject]

infix operator ~~ { associativity left }
infix operator ~~? { associativity left }

// Generic

func ~~ <A, B, C>(t: (A -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {
    if let (con, json) = t, a = json[key] as? A { return (con(a), json) }
    else { return nil }
}
func ~~ <A, B>(t: (A -> B, JSON)?, key: String) -> B? {
    if let (con, json) = t, a = json[key] as? A { return con(a) }
    else { return nil }
}
func ~~? <A, B, C>(t: (A? -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {
    if let (con, json) = t { return (con(json[key] as? A), json) }
    else { return nil }
}
func ~~? <A, B>(t: (A? -> B, JSON)?, key: String) -> B? {
    if let (con, json) = t { return con(json[key] as? A) }
    else { return nil }
}
// NOTE: collections of primitives (i. e. [String] and [String : NSNumber]) matched by generics

// Overloaded for newtypes
func cJS<A: JSONDecodable>(a: [String : AnyObject]) -> A? { return A(json: a) }

func ~~ <A: JSONDecodable, B, C>(t: (A -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {
    if let (con, json) = t, a = json[key] as? JSON, v: A = cJS(a) { return (con(v), json) }
    else { return nil }
}
func ~~ <A: JSONDecodable, B>(t: (A -> B, JSON)?, key: String) -> B? {
    if let (con, json) = t, a = json[key] as? JSON, v: A = cJS(a) { return con(v) }
    else { return nil }
}
func ~~? <A: JSONDecodable, B, C>(t: (A? -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {
    if let (con, json) = t {
        if let js = json[key] as? JSON {
            return (con(cJS(js)), json)
        } else { return (con(nil), json) }
    } else { return nil }
}
func ~~? <A: JSONDecodable, B>(t: (A? -> B, JSON)?, key: String) -> B? {
    if let (con, json) = t {
        if let js = json[key] as? JSON {
            return con(cJS(js))
        } else { return con(nil) }
    } else { return nil }
}
func ~~ <A: JSONDecodable, B, C>(t: ([A] -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {
    if let (con, json) = t, a = json[key] as? [JSON] {
        var arr = [A]()
        for js in a { if let v: A = cJS(js) { arr.append(v) } }
        return (con(arr), json)
    } else { return nil }
}
func ~~ <A: JSONDecodable, B>(t: ([A] -> B, JSON)?, key: String) -> B? {
    if let (con, json) = t, a = json[key] as? [JSON] {
        var arr = [A]()
        for js in a { if let v: A = cJS(js) { arr.append(v) } }
        return con(arr)
    } else { return nil }
}
func ~~? <A: JSONDecodable, B, C>(t: ([A]? -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {
    if let (con, json) = t {
        return (con(json[key].map({
            var arr = [A]()
            if let a = $0 as? [JSON] {
                for js: JSON in a { if let v: A = cJS(js) { arr.append(v) } }
            }
            return arr
        })), json)
    } else { return nil }
}
func ~~? <A: JSONDecodable, B>(t: ([A]? -> B, JSON)?, key: String) -> B? {
    if let (con, json) = t {
        return con(json[key].map({
            var arr = [A]()
            if let a = $0 as? [JSON] {
                for js: JSON in a { if let v: A = cJS(js) { arr.append(v) } }
            }
            return arr
        }))
    } else { return nil }
}
func ~~ <A: JSONDecodable, B, C>(t: ([String : A] -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {
    if let (con, json) = t, jsond = json[key] as? [String : JSON] {
        var dict = [String : A]();
        for (key, js) in jsond { dict[key] = cJS(js) }
        return (con(dict), json)
    } else { return nil }
}
func ~~ <A: JSONDecodable, B>(t: ([String : A] -> B, JSON)?, key: String) -> B? {
    if let (con, json) = t, jsond = json[key] as? [String : JSON] {
        var dict = [String : A]();
        for (key, js) in jsond { dict[key] = cJS(js) }
        return con(dict)
    } else { return nil }
}
func ~~? <A: JSONDecodable, B, C>(t: ([String : A]? -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {
    if let (con, json) = t {
        if let jsond = json[key] as? [String : JSON] {
            var dict = [String : A]();
            for (key, js) in jsond { dict[key] = cJS(js) }
            return (con(dict), json)
        } else { return (con(nil), json) }
    } else { return nil }
}
func ~~? <A: JSONDecodable, B>(t: ([String : A]? -> B, JSON)?, key: String) -> B? {
    if let (con, json) = t {
        if let jsond = json[key] as? [String : JSON] {
            var dict = [String : A]();
            for (key, js) in jsond { dict[key] = cJS(js) }
            return con(dict)
        } else { return con(nil) }
    } else { return nil }
}
