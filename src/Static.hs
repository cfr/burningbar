module Static where

import Util

intDefs protoName name methods = "\nimport Foundation\n\
\\n\
\public func idTf<T>(a: T, json: [String : AnyObject]) -> T { return a }\n\
\public protocol " ⧺ protoName ⧺ " {\n\
\    typealias CancellationToken\n\
\    func cancel(token: CancellationToken)\n\
\    func cast(method: String, arguments: [String : AnyObject])\n\
\    func listen(event: String,\n\
\                listener: [String : AnyObject] -> Void) -> CancellationToken\n\
\    func call(method: String, arguments: [String : AnyObject],\n\
\              completion: [String : AnyObject] -> Void) -> CancellationToken\n\
\}\n\n\
\public class " ⧺ name ⧺ " <T: " ⧺ protoName ⧺ "> {\n\
\    public func cancel(token: T.CancellationToken) { transport.cancel(token) }\n\
\    public func listen(event: String,\n\
\        listener: [String : AnyObject] -> Void) -> T.CancellationToken \
\{ return transport.listen(event, listener: listener) }\n\
\    public func cast(method: String, arguments: [String : AnyObject]) \
\{ transport.cast(method, arguments: arguments) }\n\
\    func call(method: String, arguments: [String : AnyObject],\n\
\              completion: [String : AnyObject] -> Void) -> T.CancellationToken\n\
\      { return transport.call(method, arguments: arguments, completion: completion) }\n\
\    public init(transport: T) { self.transport = transport }\n\
\    public let transport: T\n\n" ⧺ methods ⧺ "}\n"

entDefs = "\n\
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
\infix operator ~~ { associativity left }\n\
\infix operator ~~? { associativity left }\n\
\// Generic\n\
\func ~~ <A, B, C>(t: (A -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {\n\
\    if let (con, json) = t, a = json[key] as? A { return (con(a), json) }\n\
\    else { return nil }\n\
\}\n\
\func ~~ <A, B>(t: (A -> B, JSON)?, key: String) -> B? {\n\
\    if let (con, json) = t, a = json[key] as? A { return con(a) }\n\
\    else { return nil }\n\
\}\n\
\func ~~? <A, B, C>(t: (A? -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {\n\
\    if let (con, json) = t { return (con(json[key] as? A), json) }\n\
\    else { return nil }\n\
\}\n\
\func ~~? <A, B>(t: (A? -> B, JSON)?, key: String) -> B? {\n\
\    if let (con, json) = t { return con(json[key] as? A) }\n\
\    else { return nil }\n\
\}\n\
\// NOTE: collections of primitives (i. e. [String] and [String : NSNumber]) matched by generics\n\
\// Overloaded for newtypes\n\
\func cJS<A: JSONDecodable>(a: [String : AnyObject]) -> A? { return A(json: a) }\n\
\\n\
\func ~~ <A: JSONDecodable, B, C>(t: (A -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {\n\
\    if let (con, json) = t, a = json[key] as? JSON, v: A = cJS(a) { return (con(v), json) }\n\
\    else { return nil }\n\
\}\n\
\func ~~ <A: JSONDecodable, B>(t: (A -> B, JSON)?, key: String) -> B? {\n\
\    if let (con, json) = t, a = json[key] as? JSON, v: A = cJS(a) { return con(v) }\n\
\    else { return nil }\n\
\}\n\
\func ~~? <A: JSONDecodable, B, C>(t: (A? -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {\n\
\    if let (con, json) = t {\n\
\        if let js = json[key] as? JSON {\n\
\            return (con(cJS(js)), json)\n\
\        } else { return (con(nil), json) }\n\
\    } else { return nil }\n\
\}\n\
\func ~~? <A: JSONDecodable, B>(t: (A? -> B, JSON)?, key: String) -> B? {\n\
\    if let (con, json) = t {\n\
\        if let js = json[key] as? JSON {\n\
\            return con(cJS(js))\n\
\        } else { return con(nil) }\n\
\    } else { return nil }\n\
\}\n\
\func ~~ <A: JSONDecodable, B, C>(t: ([A] -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {\n\
\    if let (con, json) = t, a = json[key] as? [JSON] {\n\
\        var arr = [A]()\n\
\        for js in a { if let v: A = cJS(js) { arr.append(v) } }\n\
\        return (con(arr), json)\n\
\    } else { return nil }\n\
\}\n\
\func ~~ <A: JSONDecodable, B>(t: ([A] -> B, JSON)?, key: String) -> B? {\n\
\    if let (con, json) = t, a = json[key] as? [JSON] {\n\
\        var arr = [A]()\n\
\        for js in a { if let v: A = cJS(js) { arr.append(v) } }\n\
\        return con(arr)\n\
\    } else { return nil }\n\
\}\n\
\func ~~? <A: JSONDecodable, B, C>(t: ([A]? -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {\n\
\    if let (con, json) = t {\n\
\        return (con(json[key].map({\n\
\            var arr = [A]()\n\
\            if let a = $0 as? [JSON] {\n\
\                for js: JSON in a { if let v: A = cJS(js) { arr.append(v) } }\n\
\            }\n\
\            return arr\n\
\        })), json)\n\
\    } else { return nil }\n\
\}\n\
\func ~~? <A: JSONDecodable, B>(t: ([A]? -> B, JSON)?, key: String) -> B? {\n\
\    if let (con, json) = t {\n\
\        return con(json[key].map({\n\
\            var arr = [A]()\n\
\            if let a = $0 as? [JSON] {\n\
\                for js: JSON in a { if let v: A = cJS(js) { arr.append(v) } }\n\
\            }\n\
\            return arr\n\
\        }))\n\
\    } else { return nil }\n\
\}\n\
\func ~~ <A: JSONDecodable, B, C>(t: ([String : A] -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {\n\
\    if let (con, json) = t, jsond = json[key] as? [String : JSON] {\n\
\        var dict = [String : A]();\n\
\        for (key, js) in jsond { dict[key] = cJS(js) as A? }\n\
\        return (con(dict), json)\n\
\    } else { return nil }\n\
\}\n\
\func ~~ <A: JSONDecodable, B>(t: ([String : A] -> B, JSON)?, key: String) -> B? {\n\
\    if let (con, json) = t, jsond = json[key] as? [String : JSON] {\n\
\        var dict = [String : A]();\n\
\        for (key, js) in jsond { dict[key] = cJS(js) as A? }\n\
\        return con(dict)\n\
\    } else { return nil }\n\
\}\n\
\func ~~? <A: JSONDecodable, B, C>(t: ([String : A]? -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {\n\
\    if let (con, json) = t {\n\
\        if let jsond = json[key] as? [String : JSON] {\n\
\            var dict = [String : A]();\n\
\            for (key, js) in jsond { dict[key] = cJS(js) as A? }\n\
\            return (con(dict), json)\n\
\        } else { return (con(nil), json) }\n\
\    } else { return nil }\n\
\}\n\
\func ~~? <A: JSONDecodable, B>(t: ([String : A]? -> B, JSON)?, key: String) -> B? {\n\
\    if let (con, json) = t {\n\
\        if let jsond = json[key] as? [String : JSON] {\n\
\            var dict = [String : A]();\n\
\            for (key, js) in jsond { dict[key] = cJS(js) as A? }\n\
\            return con(dict)\n\
\        } else { return con(nil) }\n\
\    } else { return nil }\n\
\}\n"

overloaded = "// Overloaded for JSON values\n\
\public protocol JSONValueDecodable {\n\
\    static func fromJSONVal<A>(jv: AnyObject) -> A // NOTE: returns default value\n\
\    static func fromOptJSONVal<A>(ojv: AnyObject?) -> A?\n\
\}\n\
\func toInt(o: AnyObject) -> Int {\n\
\    if let o = o as? Int { return o }\n\
\    if let o = o as? Bool { return Int(o) }\n\
\    if let o = o as? String { return o.toInt() ?? 0 }\n\
\    if let o = o as? Float { return Int(o) }\n\
\    if let o = o as? NSNumber { return o.integerValue }\n\
\    return 0\n\
\}\n\
\extension Int {\n\
\    static func fromJSONVal(js: AnyObject) -> Int { return toInt(js) }\n\
\    static func fromOptJSONVal(jso: AnyObject?) -> Int? {\n\
\        if let js: AnyObject = jso { return toInt(js) } else { return nil }\n\
\    }\n\
\}\n\
\func toNSDate(o: AnyObject) -> NSDate {\n\
\    struct Static { static var df = NSDateFormatter() { didSet {\n\
\        df.locale = NSLocale(localeIdentifier: \"en_US_POSIX\")\n\
\        df.timeZone = .localTimeZone()\n\
\        df.dateFormat = \"yyyy-MM-dd'T'HH:mm:ssZZZZZ\" } } }\n\
\    if let o = o as? Int { return NSDate(timeIntervalSince1970: Double(o)) }\n\
\    if let o = o as? Float { return NSDate(timeIntervalSince1970: Double(o)) }\n\
\    if let o = o as? Double { return NSDate(timeIntervalSince1970: o) }\n\
\    if let o = o as? String { if let d = Static.df.dateFromString(o) { return d } }\n\
\    return NSDate()\n\
\}\n\
\extension NSDate {\n\
\    static func fromJSONVal(js: AnyObject) -> NSDate { return toNSDate(js) }\n\
\    static func fromOptJSONVal(jso: AnyObject?) -> NSDate? {\n\
\        if let js: AnyObject = jso { return toNSDate(js) } else { return nil }\n\
\    }\n\
\}\n\
\extension String {\n\
\    static func fromJSONVal(js: AnyObject) -> String { return js.description }\n\
\    static func fromOptJSONVal(jso: AnyObject?) -> String? {\n\
\        if let js: AnyObject = jso { return js.description } else { return nil }\n\
\    }\n\
\}\n\
\func toBool(o: AnyObject) -> Bool {\n\
\    if let o = o as? Bool { return o }\n\
\    if let o = o as? String { switch o.lowercaseString {\n\
\    case \"true\", \"yes\", \"1\": return true\n\
\    default: return false } }\n\
\    if let o = o as? Int { return o > 0 }\n\
\    if let o = o as? NSNumber { return o.boolValue }\n\
\    return false\n\
\}\n\
\extension Bool {\n\
\    static func fromJSONVal(js: AnyObject) -> Bool { return toBool(js) }\n\
\    static func fromOptJSONVal(jso: AnyObject?) -> Bool? {\n\
\        if let js: AnyObject = jso { return toBool(js) } else { return nil }\n\
\    }\n\
\}\n\
\func cJSV<A: JSONValueDecodable>(a: AnyObject) -> A { return A.fromJSONVal(a) }\n\
\func cJSVO<A: JSONValueDecodable>(a: AnyObject?) -> A? { return A.fromOptJSONVal(a) }\n\
\\n\
\func ~~ <A: JSONValueDecodable, B, C>(t: (A -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {\n\
\    if let (con, json) = t, a: AnyObject = json[key] { return (con(cJSV(a)), json) }\n\
\    else { return nil }\n\
\}\n\
\func ~~ <A: JSONValueDecodable, B>(t: (A -> B, JSON)?, key: String) -> B? {\n\
\    if let (con, json) = t, a: AnyObject = json[key] { return con(cJSV(a)) }\n\
\    else { return nil }\n\
\}\n\
\func ~~? <A: JSONValueDecodable, B, C>(t: (A? -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {\n\
\    if let (con, json) = t { return (con(cJSVO(json[key])), json) }\n\
\    else { return nil }\n\
\}\n\
\func ~~? <A: JSONValueDecodable, B>(t: (A? -> B, JSON)?, key: String) -> B? {\n\
\    if let (con, json) = t { return con(cJSVO(json[key])) }\n\
\    else { return nil }\n\
\}\n\
\func ~~ <A: JSONValueDecodable, B, C>(t: ([A] -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {\n\
\    if let (con, json) = t, a = json[key] as? [AnyObject] { return (con(a.map(cJSV)), json) }\n\
\    else { return nil }\n\
\}\n\
\func ~~ <A: JSONValueDecodable, B>(t: ([A] -> B, JSON)?, key: String) -> B? {\n\
\    if let (con, json) = t, a = json[key] as? [AnyObject] { return con(a.map(cJSV)) }\n\
\    else { return nil }\n\
\}\n\
\func ~~? <A: JSONValueDecodable, B, C>(t: ([A]? -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {\n\
\    if let (con, json) = t {\n\
\        return (con(json[key].map({\n\
\            if let arr = $0 as? [AnyObject] { return arr.map(cJSV)}\n\
\            else { return [] }\n\
\        })), json)\n\
\    } else { return nil }\n\
\}\n\
\func ~~? <A: JSONValueDecodable, B>(t: ([A]? -> B, JSON)?, key: String) -> B? {\n\
\    if let (con, json) = t {\n\
\        return con(json[key].map({\n\
\            if let arr = $0 as? [AnyObject] { return arr.map(cJSV) }\n\
\            else { return [] } // invalid json\n\
\        }))\n\
\    } else { return nil }\n\
\}\n\
\func ~~ <A: JSONValueDecodable, B, C>(t: ([String : A] -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {\n\
\    if let (con, json) = t, jsond = json[key] as? [String : AnyObject] {\n\
\        var dict = [String : A]();\n\
\        for (key, val) in jsond { dict[key] = cJSV(val) as A }\n\
\        return (con(dict), json)\n\
\    } else { return nil }\n\
\}\n\
\func ~~ <A: JSONValueDecodable, B>(t: ([String : A] -> B, JSON)?, key: String) -> B? {\n\
\    if let (con, json) = t, jsond = json[key] as? [String : AnyObject] {\n\
\        var dict = [String : A]();\n\
\        for (key, val) in jsond { dict[key] = cJSV(val) as A }\n\
\        return con(dict)\n\
\    } else { return nil }\n\
\}\n\
\func ~~? <A: JSONValueDecodable, B, C>(t: ([String : A]? -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {\n\
\    if let (con, json) = t {\n\
\        if let jsond = json[key] as? [String : AnyObject] {\n\
\            var dict = [String : A]();\n\
\            for (key, val) in jsond { dict[key] = cJSV(val) as A }\n\
\            return (con(dict), json)\n\
\        } else { return (con(nil), json) }\n\
\    } else { return nil }\n\
\}\n\
\func ~~? <A: JSONValueDecodable, B>(t: ([String : A]? -> B, JSON)?, key: String) -> B? {\n\
\    if let (con, json) = t {\n\
\        if let jsond = json[key] as? [String : AnyObject] {\n\
\            var dict = [String : A]();\n\
\            for (key, val) in jsond { dict[key] = cJSV(val) as A }\n\
\            return con(dict)\n\
\        } else { return con(nil) }\n\
\    } else { return nil }\n\
\}\n"
