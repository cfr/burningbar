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
}

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
// Dynamicity Shield

func ~~ <B, C>(t: (String -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {
    if let (con, json) = t, a: AnyObject = json[key] { return (con(toString(a)), json) }
    else { return nil }
}
func ~~ <B>(t: (String -> B, JSON)?, key: String) -> B? {
    if let (con, json) = t, a: AnyObject = json[key] { return con(toString(a)) }
    else { return nil }
}
func ~~? <B, C>(t: (String? -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {
    if let (con, json) = t { return (con(json[key].map(toString)), json) }
    else { return nil }
}
func ~~? <B>(t: (String? -> B, JSON)?, key: String) -> B? {
    if let (con, json) = t { return con(json[key].map(toString)) }
    else { return nil }
}
func ~~ <B, C>(t: ([String] -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {
    if let (con, json) = t, a = json[key] as? [AnyObject] { return (con(a.map(toString)), json) }
    else { return nil }
}
func ~~ <B>(t: ([String] -> B, JSON)?, key: String) -> B? {
    if let (con, json) = t, a = json[key] as? [AnyObject] { return con(a.map(toString)) }
    else { return nil }
}
func ~~? <B, C>(t: ([String]? -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {
    if let (con, json) = t { return (con(json[key].map({ if let arr = $0 as? [AnyObject] { return arr.map(toString)} else { return [] } })), json) }
    else { return nil }
}
func ~~? <B>(t: ([String]? -> B, JSON)?, key: String) -> B? {
    if let (con, json) = t { return con(json[key].map({ if let arr = $0 as? [AnyObject] { return arr.map(toString)} else { return [] } })) }
    else { return nil }
}
func ~~ <B, C>(t: ([String : String] -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {
    if let (con, json) = t, jsond = json[key] as? [String : AnyObject] {
        var dict = [String : String](); for (key, val) in jsond { dict[key] = toString(val) }
        return (con(dict), json)
    }
    else { return nil }
}
func ~~ <B>(t: ([String : String] -> B, JSON)?, key: String) -> B? {
    if let (con, json) = t, jsond = json[key] as? [String : AnyObject] {
        var dict = [String : String](); for (key, val) in jsond { dict[key] = toString(val) }
        return con(dict)
    }
    else { return nil }
}
func ~~? <B, C>(t: ([String : String]? -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {
    if let (con, json) = t, jsond = json[key] as? [String : AnyObject] {
        var dict = [String : String](); for (key, val) in jsond { dict[key] = toString(val) }
        return (con(dict), json)
    }
    else { return nil }
}
func ~~? <B>(t: ([String : String]? -> B, JSON)?, key: String) -> B? {
    if let (con, json) = t, jsond = json[key] as? [String : AnyObject] {
        var dict = [String : String](); for (key, val) in jsond { dict[key] = toString(val) }
        return con(dict)
    }
    else { return nil }
}

func toInt(o: AnyObject) -> Int {
    if let o = o as? Int { return o }
    if let o = o as? Bool { return Int(o) }
    if let o = o as? String { return o.toInt() ?? 0 }
    if let o = o as? Float { return Int(o) }
    if let o = o as? NSNumber { return o.integerValue }
    return 0
}
func ~~ <B, C>(t: (Int -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {
    if let (con, json) = t, a: AnyObject = json[key] { return (con(toInt(a)), json) }
    else { return nil }
}
func ~~ <B>(t: (Int -> B, JSON)?, key: String) -> B? {
    if let (con, json) = t, a: AnyObject = json[key] { return con(toInt(a)) }
    else { return nil }
}
func ~~? <B, C>(t: (Int? -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {
    if let (con, json) = t { return (con(json[key].map(toInt)), json) }
    else { return nil }
}
func ~~? <B>(t: (Int? -> B, JSON)?, key: String) -> B? {
    if let (con, json) = t { return con(json[key].map(toInt)) }
    else { return nil }
}
func ~~ <B, C>(t: ([Int] -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {
    if let (con, json) = t, a = json[key] as? [AnyObject] { return (con(a.map(toInt)), json) }
    else { return nil }
}
func ~~ <B>(t: ([Int] -> B, JSON)?, key: String) -> B? {
    if let (con, json) = t, a = json[key] as? [AnyObject] { return con(a.map(toInt)) }
    else { return nil }
}
func ~~? <B, C>(t: ([Int]? -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {
    if let (con, json) = t { return (con(json[key].map({ if let arr = $0 as? [AnyObject] { return arr.map(toInt)} else { return [] } })), json) }
    else { return nil }
}
func ~~? <B>(t: ([Int]? -> B, JSON)?, key: String) -> B? {
    if let (con, json) = t { return con(json[key].map({ if let arr = $0 as? [AnyObject] { return arr.map(toInt)} else { return [] } })) }
    else { return nil }
}
func ~~ <B, C>(t: ([String : Int] -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {
    if let (con, json) = t, jsond = json[key] as? [String : AnyObject] {
        var dict = [String : Int](); for (key, val) in jsond { dict[key] = toInt(val) }
        return (con(dict), json)
    }
    else { return nil }
}
func ~~ <B>(t: ([String : Int] -> B, JSON)?, key: String) -> B? {
    if let (con, json) = t, jsond = json[key] as? [String : AnyObject] {
        var dict = [String : Int](); for (key, val) in jsond { dict[key] = toInt(val) }
        return con(dict)
    }
    else { return nil }
}
func ~~? <B, C>(t: ([String : Int]? -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {
    if let (con, json) = t, jsond = json[key] as? [String : AnyObject] {
        var dict = [String : Int](); for (key, val) in jsond { dict[key] = toInt(val) }
        return (con(dict), json)
    }
    else { return nil }
}
func ~~? <B>(t: ([String : Int]? -> B, JSON)?, key: String) -> B? {
    if let (con, json) = t, jsond = json[key] as? [String : AnyObject] {
        var dict = [String : Int](); for (key, val) in jsond { dict[key] = toInt(val) }
        return con(dict)
    }
    else { return nil }
}

func toBool(o: AnyObject) -> Bool {
    if let o = o as? Bool { return o }
    if let o = o as? String { switch o.lowercaseString {
        case "true", "yes", "1": return true
        default: return false }
    }
    if let o = o as? Int { return o > 0 }
    if let o = o as? NSNumber { return o.boolValue }
    return false
}
func ~~ <B, C>(t: (Bool -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {
    if let (con, json) = t, a: AnyObject = json[key] { return (con(toBool(a)), json) }
    else { return nil }
}
func ~~ <B>(t: (Bool -> B, JSON)?, key: String) -> B? {
    if let (con, json) = t, a: AnyObject = json[key] { return con(toBool(a)) }
    else { return nil }
}
func ~~? <B, C>(t: (Bool? -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {
    if let (con, json) = t { return (con(json[key].map(toBool)), json) }
    else { return nil }
}
func ~~? <B>(t: (Bool? -> B, JSON)?, key: String) -> B? {
    if let (con, json) = t { return con(json[key].map(toBool)) }
    else { return nil }
}
func ~~ <B, C>(t: ([Bool] -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {
    if let (con, json) = t, a = json[key] as? [AnyObject] { return (con(a.map(toBool)), json) }
    else { return nil }
}
func ~~ <B>(t: ([Bool] -> B, JSON)?, key: String) -> B? {
    if let (con, json) = t, a = json[key] as? [AnyObject] { return con(a.map(toBool)) }
    else { return nil }
}
func ~~? <B, C>(t: ([Bool]? -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {
    if let (con, json) = t { return (con(json[key].map({ if let arr = $0 as? [AnyObject] { return arr.map(toBool)} else { return [] } })), json) }
    else { return nil }
}
func ~~? <B>(t: ([Bool]? -> B, JSON)?, key: String) -> B? {
    if let (con, json) = t { return con(json[key].map({ if let arr = $0 as? [AnyObject] { return arr.map(toBool)} else { return [] } })) }
    else { return nil }
}
func ~~ <B, C>(t: ([String : Bool] -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {
    if let (con, json) = t, jsond = json[key] as? [String : AnyObject] {
        var dict = [String : Bool](); for (key, val) in jsond { dict[key] = toBool(val) }
        return (con(dict), json)
    }
    else { return nil }
}
func ~~ <B>(t: ([String : Bool] -> B, JSON)?, key: String) -> B? {
    if let (con, json) = t, jsond = json[key] as? [String : AnyObject] {
        var dict = [String : Bool](); for (key, val) in jsond { dict[key] = toBool(val) }
        return con(dict)
    }
    else { return nil }
}
func ~~? <B, C>(t: ([String : Bool]? -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {
    if let (con, json) = t, jsond = json[key] as? [String : AnyObject] {
        var dict = [String : Bool](); for (key, val) in jsond { dict[key] = toBool(val) }
        return (con(dict), json)
    }
    else { return nil }
}
func ~~? <B>(t: ([String : Bool]? -> B, JSON)?, key: String) -> B? {
    if let (con, json) = t, jsond = json[key] as? [String : AnyObject] {
        var dict = [String : Bool](); for (key, val) in jsond { dict[key] = toBool(val) }
        return con(dict)
    }
    else { return nil }
}
