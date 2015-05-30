module Static where

import Util

intDefs protoName name methods = "import Foundation\n\
\\n\
\public func idTf<T>(a: T, json: [String : AnyObject]) -> T { return a }\n\
\public protocol " ⧺ protoName ⧺ " {\n\
\    typealias CancellationToken\n\
\    func cancel(token: CancellationToken)\n\
\    func cast(method: String, arguments: [String : AnyObject])\n\
\    func listen(event: String,\n\
\                completion: [String : AnyObject] -> Void) -> CancellationToken\n\
\    func call(method: String, arguments: [String : AnyObject],\n\
\              completion: [String : AnyObject] -> Void) -> CancellationToken\n\
\}\n\n\
\public class " ⧺ name ⧺ " <T: " ⧺ protoName ⧺ "> {\n\
\    public func cancel(token: T.CancellationToken) { transport.cancel(token) }\n\
\    public func listen(event: String,\n\
\        completion: [String : AnyObject] -> Void) -> T.CancellationToken \
\{ return transport.listen(event, completion: completion) }\n\
\    public func cast(method: String, arguments: [String : AnyObject]) \
\{ transport.cast(method, arguments: arguments) }\n\
\    public init(transport: T) { self.transport = transport }\n\
\    public let transport: T\n\n" ⧺ methods ⧺ "}\n"

entDefs = "import Foundation\n\
\public protocol JSONEncodable {\n\
\    var json: [String : AnyObject] { get }\n\
\    var Name: String { get }\n\
\}\n\
\public protocol JSONDecodable {\n\
\    init?(json: [String : AnyObject])\n\
\}\n\
\\n\
\// JSON mapping operators\n\
\\n\
\typealias JSON = [String : AnyObject]\n\
\\n\
\infix operator ~~ { associativity left }\n\
\infix operator ~~? { associativity left }\n\
\\n\
\func ~~ <A, B, C>(t: (A -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {\n\
\    if let (con, json) = t, a = json[key] as? A { return (con(a), json) }\n\
\    else { return nil }\n\
\}\n\
\\n\
\func ~~ <A: JSONDecodable, B, C>(t: ([A] -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {\n\
\    if let (con, json) = t, jsons = json[key] as? [JSON] {\n\
\        var array = [A](); for json in jsons { if let a = A(json: json) { array.append(a) } }\n\
\        return (con(array), json)\n\
\    } else { return nil }\n\
\}\n\
\\n\
\func ~~ <A: JSONDecodable, B, C>(t: ([String : A] -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {\n\
\    if let (con, json) = t, jsons = json[key] as? [String : JSON] {\n\
\        var dict = [String : A](); for (key, json) in jsons { if let a = A(json: json) { dict[key] = a } }\n\
\        return (con(dict), json)\n\
\    } else { return nil }\n\
\}\n\
\\n\
\func ~~ <A, B>(t: (A -> B, JSON)?, key: String) -> B? {\n\
\    if let (con, json) = t, a = json[key] as? A { return con(a) }\n\
\    else { return nil }\n\
\}\n\
\\n\
\func ~~ <A: JSONDecodable, B>(t: ([A] -> B, JSON)?, key: String) -> B? {\n\
\    if let (con, json) = t, jsons = json[key] as? [JSON] {\n\
\        var array = [A](); for json in jsons { if let a = A(json: json) { array.append(a) } }\n\
\        return con(array)\n\
\    } else { return nil }\n\
\}\n\
\\n\
\func ~~ <A: JSONDecodable, B>(t: ([String : A] -> B, JSON)?, key: String) -> B? {\n\
\    if let (con, json) = t, jsons = json[key] as? [String : JSON] {\n\
\        var dict = [String : A](); for (key, json) in jsons { if let a = A(json: json) { dict[key] = a } }\n\
\        return con(dict)\n\
\    } else { return nil }\n\
\}\n\
\\n\
\func ~~? <A, B, C>(t: (A? -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {\n\
\    if let (con, json) = t { return (con(json[key] as? A), json) }\n\
\    else { return nil }\n\
\}\n\
\\n\
\func ~~? <A: JSONDecodable, B, C>(t: (A? -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {\n\
\    if let (con, json) = t {\n\
\        if let ajson = json[key] as? JSON { return (con(A(json: ajson)), json) }\n\
\        else { return (con(nil), json) }\n\
\    } else { return nil }\n\
\}\n\
\\n\
\func ~~? <A: JSONDecodable, B, C>(t: ([A]? -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {\n\
\    if let (con, json) = t {\n\
\        if let jsons = json[key] as? [JSON] {\n\
\            var array = [A](); for json in jsons { if let a = A(json: json) { array.append(a) } }\n\
\            return (con(array), json)\n\
\        } else { return (con(nil), json) }\n\
\    } else { return nil }\n\
\}\n\
\\n\
\func ~~? <A: JSONDecodable, B, C>(t: ([String : A]? -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {\n\
\    if let (con, json) = t { if let jsons = json[key] as? [String : JSON] {\n\
\            var dict = [String : A](); for (key, json) in jsons { if let a = A(json: json) { dict[key] = a } }\n\
\            return (con(dict), json)\n\
\        } else { return (con(nil), json) }\n\
\    } else { return nil }\n\
\}\n\
\\n\
\func ~~? <A, B>(t: (A? -> B, JSON)?, key: String) -> B? {\n\
\    if let (con, json) = t { return con(json[key] as? A) }\n\
\    else { return nil }\n\
\}\n\
\\n\
\func ~~? <A: JSONDecodable, B>(t: (A? -> B, JSON)?, key: String) -> B? {\n\
\    if let (con, json) = t {\n\
\        if let ajson = json[key] as? JSON { return con(A(json: ajson)) }\n\
\        else { return con(nil) }\n\
\    } else { return nil }\n\
\\n\
\}\n\
\\n\
\func ~~? <A: JSONDecodable, B>(t: ([A]? -> B, JSON)?, key: String) -> B? {\n\
\    if let (con, json) = t { if let jsons = json[key] as? [JSON] {\n\
\            var array = [A](); for json in jsons { if let a = A(json: json) { array.append(a) } }\n\
\            return con(array)\n\
\        } else { return con(nil) }\n\
\    } else { return nil }\n\
\}\n\
\\n\
\func ~~? <A: JSONDecodable, B>(t: ([String : A]? -> B, JSON)?, key: String) -> B? {\n\
\    if let (con, json) = t { if let jsons = json[key] as? [String : JSON] {\n\
\            var dict = [String : A](); for (key, json) in jsons { if let a = A(json: json) { dict[key] = a } }\n\
\            return con(dict)\n\
\        } else { return con(nil) }\n\
\    } else { return nil }\n\
\}\n\n"

dynamicityShield = "// Dynamicity Shield\n\
\\n\
\func ~~ <B, C>(t: (String -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {\n\
\    if let (con, json) = t, a: AnyObject = json[key] { return (con(toString(a)), json) }\n\
\    else { return nil }\n\
\}\n\
\func ~~ <B>(t: (String -> B, JSON)?, key: String) -> B? {\n\
\    if let (con, json) = t, a: AnyObject = json[key] { return con(toString(a)) }\n\
\    else { return nil }\n\
\}\n\
\func ~~? <B, C>(t: (String? -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {\n\
\    if let (con, json) = t { return (con(json[key].map(toString)), json) }\n\
\    else { return nil }\n\
\}\n\
\func ~~? <B>(t: (String? -> B, JSON)?, key: String) -> B? {\n\
\    if let (con, json) = t { return con(json[key].map(toString)) }\n\
\    else { return nil }\n\
\}\n\
\func ~~ <B, C>(t: ([String] -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {\n\
\    if let (con, json) = t, a = json[key] as? [AnyObject] { return (con(a.map(toString)), json) }\n\
\    else { return nil }\n\
\}\n\
\func ~~ <B>(t: ([String] -> B, JSON)?, key: String) -> B? {\n\
\    if let (con, json) = t, a = json[key] as? [AnyObject] { return con(a.map(toString)) }\n\
\    else { return nil }\n\
\}\n\
\func ~~? <B, C>(t: ([String]? -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {\n\
\    if let (con, json) = t { return (con(json[key].map({ if let arr = $0 as? [AnyObject] { return arr.map(toString)} else { return [] } })), json) }\n\
\    else { return nil }\n\
\}\n\
\func ~~? <B>(t: ([String]? -> B, JSON)?, key: String) -> B? {\n\
\    if let (con, json) = t { return con(json[key].map({ if let arr = $0 as? [AnyObject] { return arr.map(toString)} else { return [] } })) }\n\
\    else { return nil }\n\
\}\n\
\func ~~ <B, C>(t: ([String : String] -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {\n\
\    if let (con, json) = t, jsond = json[key] as? [String : AnyObject] {\n\
\        var dict = [String : String](); for (key, val) in jsond { dict[key] = toString(val) }\n\
\        return (con(dict), json)\n\
\    }\n\
\    else { return nil }\n\
\}\n\
\func ~~ <B>(t: ([String : String] -> B, JSON)?, key: String) -> B? {\n\
\    if let (con, json) = t, jsond = json[key] as? [String : AnyObject] {\n\
\        var dict = [String : String](); for (key, val) in jsond { dict[key] = toString(val) }\n\
\        return con(dict)\n\
\    }\n\
\    else { return nil }\n\
\}\n\
\func ~~? <B, C>(t: ([String : String]? -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {\n\
\    if let (con, json) = t, jsond = json[key] as? [String : AnyObject] {\n\
\        var dict = [String : String](); for (key, val) in jsond { dict[key] = toString(val) }\n\
\        return (con(dict), json)\n\
\    }\n\
\    else { return nil }\n\
\}\n\
\func ~~? <B>(t: ([String : String]? -> B, JSON)?, key: String) -> B? {\n\
\    if let (con, json) = t, jsond = json[key] as? [String : AnyObject] {\n\
\        var dict = [String : String](); for (key, val) in jsond { dict[key] = toString(val) }\n\
\        return con(dict)\n\
\    }\n\
\    else { return nil }\n\
\}\n\
\\n\
\func toInt(o: AnyObject) -> Int {\n\
\    if let o = o as? Int { return o }\n\
\    if let o = o as? Bool { return Int(o) }\n\
\    if let o = o as? String { return o.toInt() ?? 0 }\n\
\    if let o = o as? Float { return Int(o) }\n\
\    if let o = o as? NSNumber { return o.integerValue }\n\
\    return 0\n\
\}\n\
\func ~~ <B, C>(t: (Int -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {\n\
\    if let (con, json) = t, a: AnyObject = json[key] { return (con(toInt(a)), json) }\n\
\    else { return nil }\n\
\}\n\
\func ~~ <B>(t: (Int -> B, JSON)?, key: String) -> B? {\n\
\    if let (con, json) = t, a: AnyObject = json[key] { return con(toInt(a)) }\n\
\    else { return nil }\n\
\}\n\
\func ~~? <B, C>(t: (Int? -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {\n\
\    if let (con, json) = t { return (con(json[key].map(toInt)), json) }\n\
\    else { return nil }\n\
\}\n\
\func ~~? <B>(t: (Int? -> B, JSON)?, key: String) -> B? {\n\
\    if let (con, json) = t { return con(json[key].map(toInt)) }\n\
\    else { return nil }\n\
\}\n\
\func ~~ <B, C>(t: ([Int] -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {\n\
\    if let (con, json) = t, a = json[key] as? [AnyObject] { return (con(a.map(toInt)), json) }\n\
\    else { return nil }\n\
\}\n\
\func ~~ <B>(t: ([Int] -> B, JSON)?, key: String) -> B? {\n\
\    if let (con, json) = t, a = json[key] as? [AnyObject] { return con(a.map(toInt)) }\n\
\    else { return nil }\n\
\}\n\
\func ~~? <B, C>(t: ([Int]? -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {\n\
\    if let (con, json) = t { return (con(json[key].map({ if let arr = $0 as? [AnyObject] { return arr.map(toInt)} else { return [] } })), json) }\n\
\    else { return nil }\n\
\}\n\
\func ~~? <B>(t: ([Int]? -> B, JSON)?, key: String) -> B? {\n\
\    if let (con, json) = t { return con(json[key].map({ if let arr = $0 as? [AnyObject] { return arr.map(toInt)} else { return [] } })) }\n\
\    else { return nil }\n\
\}\n\
\func ~~ <B, C>(t: ([String : Int] -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {\n\
\    if let (con, json) = t, jsond = json[key] as? [String : AnyObject] {\n\
\        var dict = [String : Int](); for (key, val) in jsond { dict[key] = toInt(val) }\n\
\        return (con(dict), json)\n\
\    }\n\
\    else { return nil }\n\
\}\n\
\func ~~ <B>(t: ([String : Int] -> B, JSON)?, key: String) -> B? {\n\
\    if let (con, json) = t, jsond = json[key] as? [String : AnyObject] {\n\
\        var dict = [String : Int](); for (key, val) in jsond { dict[key] = toInt(val) }\n\
\        return con(dict)\n\
\    }\n\
\    else { return nil }\n\
\}\n\
\func ~~? <B, C>(t: ([String : Int]? -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {\n\
\    if let (con, json) = t, jsond = json[key] as? [String : AnyObject] {\n\
\        var dict = [String : Int](); for (key, val) in jsond { dict[key] = toInt(val) }\n\
\        return (con(dict), json)\n\
\    }\n\
\    else { return nil }\n\
\}\n\
\func ~~? <B>(t: ([String : Int]? -> B, JSON)?, key: String) -> B? {\n\
\    if let (con, json) = t, jsond = json[key] as? [String : AnyObject] {\n\
\        var dict = [String : Int](); for (key, val) in jsond { dict[key] = toInt(val) }\n\
\        return con(dict)\n\
\    }\n\
\    else { return nil }\n\
\}\n\
\\n\
\func toBool(o: AnyObject) -> Bool {\n\
\    if let o = o as? Bool { return o }\n\
\    if let o = o as? String { switch o.lowercaseString {\n\
\        case \"true\", \"yes\", \"1\": return true\n\
\        default: return false }\n\
\    }\n\
\    if let o = o as? Int { return o > 0 }\n\
\    if let o = o as? NSNumber { return o.boolValue }\n\
\    return false\n\
\}\n\
\func ~~ <B, C>(t: (Bool -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {\n\
\    if let (con, json) = t, a: AnyObject = json[key] { return (con(toBool(a)), json) }\n\
\    else { return nil }\n\
\}\n\
\func ~~ <B>(t: (Bool -> B, JSON)?, key: String) -> B? {\n\
\    if let (con, json) = t, a: AnyObject = json[key] { return con(toBool(a)) }\n\
\    else { return nil }\n\
\}\n\
\func ~~? <B, C>(t: (Bool? -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {\n\
\    if let (con, json) = t { return (con(json[key].map(toBool)), json) }\n\
\    else { return nil }\n\
\}\n\
\func ~~? <B>(t: (Bool? -> B, JSON)?, key: String) -> B? {\n\
\    if let (con, json) = t { return con(json[key].map(toBool)) }\n\
\    else { return nil }\n\
\}\n\
\func ~~ <B, C>(t: ([Bool] -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {\n\
\    if let (con, json) = t, a = json[key] as? [AnyObject] { return (con(a.map(toBool)), json) }\n\
\    else { return nil }\n\
\}\n\
\func ~~ <B>(t: ([Bool] -> B, JSON)?, key: String) -> B? {\n\
\    if let (con, json) = t, a = json[key] as? [AnyObject] { return con(a.map(toBool)) }\n\
\    else { return nil }\n\
\}\n\
\func ~~? <B, C>(t: ([Bool]? -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {\n\
\    if let (con, json) = t { return (con(json[key].map({ if let arr = $0 as? [AnyObject] { return arr.map(toBool)} else { return [] } })), json) }\n\
\    else { return nil }\n\
\}\n\
\func ~~? <B>(t: ([Bool]? -> B, JSON)?, key: String) -> B? {\n\
\    if let (con, json) = t { return con(json[key].map({ if let arr = $0 as? [AnyObject] { return arr.map(toBool)} else { return [] } })) }\n\
\    else { return nil }\n\
\}\n\
\func ~~ <B, C>(t: ([String : Bool] -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {\n\
\    if let (con, json) = t, jsond = json[key] as? [String : AnyObject] {\n\
\        var dict = [String : Bool](); for (key, val) in jsond { dict[key] = toBool(val) }\n\
\        return (con(dict), json)\n\
\    }\n\
\    else { return nil }\n\
\}\n\
\func ~~ <B>(t: ([String : Bool] -> B, JSON)?, key: String) -> B? {\n\
\    if let (con, json) = t, jsond = json[key] as? [String : AnyObject] {\n\
\        var dict = [String : Bool](); for (key, val) in jsond { dict[key] = toBool(val) }\n\
\        return con(dict)\n\
\    }\n\
\    else { return nil }\n\
\}\n\
\func ~~? <B, C>(t: ([String : Bool]? -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {\n\
\    if let (con, json) = t, jsond = json[key] as? [String : AnyObject] {\n\
\        var dict = [String : Bool](); for (key, val) in jsond { dict[key] = toBool(val) }\n\
\        return (con(dict), json)\n\
\    }\n\
\    else { return nil }\n\
\}\n\
\func ~~? <B>(t: ([String : Bool]? -> B, JSON)?, key: String) -> B? {\n\
\    if let (con, json) = t, jsond = json[key] as? [String : AnyObject] {\n\
\        var dict = [String : Bool](); for (key, val) in jsond { dict[key] = toBool(val) }\n\
\        return con(dict)\n\
\    }\n\
\    else { return nil }\n\
\}\n\n"
