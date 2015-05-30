module Static where

import Util

intDefs protoName name methods = "import Foundation\n\
\\n\
\public func idTf<T>(a: T) -> T { return a }\n\
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
\\n\
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
\typealias JSON = [String: AnyObject]\n\
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
\func ~~ <A: JSONDecodable, B, C>(t: ([String: A] -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {\n\
\    if let (con, json) = t, jsons = json[key] as? [String: JSON] {\n\
\        var dict = [String: A](); for (key, json) in jsons { if let a = A(json: json) { dict[key] = a } }\n\
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
\func ~~ <A: JSONDecodable, B>(t: ([String: A] -> B, JSON)?, key: String) -> B? {\n\
\    if let (con, json) = t, jsons = json[key] as? [String: JSON] {\n\
\        var dict = [String: A](); for (key, json) in jsons { if let a = A(json: json) { dict[key] = a } }\n\
\        return con(dict)\n\
\    } else { return nil }\n\
\}\n\
\\n\
\func ~~? <A, B, C>(t: (A? -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {\n\
\    if let (con, json) = t { return (con(json[key] as? A), json) }\n\
\    else { return nil }\n\
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
\func ~~? <A: JSONDecodable, B, C>(t: ([String: A]? -> B -> C, JSON)?, key: String) -> (B -> C, JSON)? {\n\
\    if let (con, json) = t { if let jsons = json[key] as? [String: JSON] {\n\
\            var dict = [String: A](); for (key, json) in jsons { if let a = A(json: json) { dict[key] = a } }\n\
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
\func ~~? <A: JSONDecodable, B>(t: ([A]? -> B, JSON)?, key: String) -> B? {\n\
\    if let (con, json) = t { if let jsons = json[key] as? [JSON] {\n\
\            var array = [A](); for json in jsons { if let a = A(json: json) { array.append(a) } }\n\
\            return con(array)\n\
\        } else { return con(nil) }\n\
\    } else { return nil }\n\
\}\n\
\\n\
\func ~~? <A: JSONDecodable, B>(t: ([String: A]? -> B, JSON)?, key: String) -> B? {\n\
\    if let (con, json) = t { if let jsons = json[key] as? [String: JSON] {\n\
\            var dict = [String: A](); for (key, json) in jsons { if let a = A(json: json) { dict[key] = a } }\n\
\            return con(dict)\n\
\        } else { return con(nil) }\n\
\    } else { return nil }\n\
\}\n\
\\n"

