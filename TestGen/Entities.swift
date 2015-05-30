// 📏🔥 Generated with http://j.mp/burnbar v0.5.30


public struct Credentials: JSONEncodable, JSONDecodable, YourProto {
    public let json: [String : AnyObject]
    public let login: String?
    public let pass: String?
    public let Name: String
    public static let Name = "Credentials"
    public static let json = "json"
    public static let login = "login"
    public static let pass = "pass"
    public init?(json: [String : AnyObject]) {
        let (c, json) = (Credentials.create(json), json)
        if let c_ = (c, json) ~~? "login" ~~? "pass" { self = c_ }
        return nil
    }
    public init(json: [String : AnyObject], login: String?, pass: String?) {
        self.json = json; self.login = login; self.pass = pass; self.Name = "Credentials"
    }
    static func create(json: [String : AnyObject])(login: String?)(pass: String?) -> Credentials {
        return Credentials(json: json, login: login, pass: pass)
    }
}

public struct User: JSONEncodable, JSONDecodable, YourProto {
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
        if let (c_, json) = (c, json)  ~~? "age" ~~ "photoURLs" ~~? "creds" ~~ "friends" {
          if let c__ = (c_, json) ~~ "name" { self = c__ }
        }
        return nil
    }
    public init(json: [String : AnyObject], age: Int?, photoURLs: [String], creds: Credentials?, friends: [String: User], name: String) {
        self.json = json; self.age = age; self.photoURLs = photoURLs; self.creds = creds; self.friends = friends; self.name = name; self.Name = "User"
    }
    static func create(json: [String : AnyObject])(age: Int?)(photoURLs: [String])(creds: Credentials?)(friends: [String: User])(name: String) -> User {
        return User(json: json, age: age, photoURLs: photoURLs, creds: creds, friends: friends, name: name)
    }
}
import Foundation
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

func ~~ <A, B, C>(t: (A -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {
    if let (con, json) = t, a = json[key] as? A { return (con(a), json) }
    else { return nil }
}

func ~~ <A: JSONDecodable, B, C>(t: ([A] -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {
    if let (con, json) = t, jsons = json[key] as? [JSON] {
        var array = [A](); for json in jsons { if let a = A(json: json) { array.append(a) } }
        return (con(array), json)
    } else { return nil }
}

func ~~ <A: JSONDecodable, B, C>(t: ([String : A] -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {
    if let (con, json) = t, jsons = json[key] as? [String : JSON] {
        var dict = [String : A](); for (key, json) in jsons { if let a = A(json: json) { dict[key] = a } }
        return (con(dict), json)
    } else { return nil }
}

func ~~ <A, B>(t: (A -> B, JSON)?, key: String) -> B? {
    if let (con, json) = t, a = json[key] as? A { return con(a) }
    else { return nil }
}

func ~~ <A: JSONDecodable, B>(t: ([A] -> B, JSON)?, key: String) -> B? {
    if let (con, json) = t, jsons = json[key] as? [JSON] {
        var array = [A](); for json in jsons { if let a = A(json: json) { array.append(a) } }
        return con(array)
    } else { return nil }
}

func ~~ <A: JSONDecodable, B>(t: ([String : A] -> B, JSON)?, key: String) -> B? {
    if let (con, json) = t, jsons = json[key] as? [String : JSON] {
        var dict = [String : A](); for (key, json) in jsons { if let a = A(json: json) { dict[key] = a } }
        return con(dict)
    } else { return nil }
}

func ~~? <A, B, C>(t: (A? -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {
    if let (con, json) = t { return (con(json[key] as? A), json) }
    else { return nil }
}

func ~~? <A: JSONDecodable, B, C>(t: (A? -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {
    if let (con, json) = t {
        if let ajson = json[key] as? JSON { return (con(A(json: ajson)), json) }
        else { return (con(nil), json) }
    } else { return nil }
}

func ~~? <A: JSONDecodable, B, C>(t: ([A]? -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {
    if let (con, json) = t {
        if let jsons = json[key] as? [JSON] {
            var array = [A](); for json in jsons { if let a = A(json: json) { array.append(a) } }
            return (con(array), json)
        } else { return (con(nil), json) }
    } else { return nil }
}

func ~~? <A: JSONDecodable, B, C>(t: ([String : A]? -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {
    if let (con, json) = t { if let jsons = json[key] as? [String : JSON] {
            var dict = [String : A](); for (key, json) in jsons { if let a = A(json: json) { dict[key] = a } }
            return (con(dict), json)
        } else { return (con(nil), json) }
    } else { return nil }
}

func ~~? <A, B>(t: (A? -> B, JSON)?, key: String) -> B? {
    if let (con, json) = t { return con(json[key] as? A) }
    else { return nil }
}

func ~~? <A: JSONDecodable, B>(t: (A? -> B, JSON)?, key: String) -> B? {
    if let (con, json) = t {
        if let ajson = json[key] as? JSON { return con(A(json: ajson)) }
        else { return con(nil) }
    } else { return nil }

}

func ~~? <A: JSONDecodable, B>(t: ([A]? -> B, JSON)?, key: String) -> B? {
    if let (con, json) = t { if let jsons = json[key] as? [JSON] {
            var array = [A](); for json in jsons { if let a = A(json: json) { array.append(a) } }
            return con(array)
        } else { return con(nil) }
    } else { return nil }
}

func ~~? <A: JSONDecodable, B>(t: ([String : A]? -> B, JSON)?, key: String) -> B? {
    if let (con, json) = t { if let jsons = json[key] as? [String : JSON] {
            var dict = [String : A](); for (key, json) in jsons { if let a = A(json: json) { dict[key] = a } }
            return con(dict)
        } else { return con(nil) }
    } else { return nil }
}

