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
\    public func cancel(token: " ⧺ protoName ⧺ ".CancellationToken) { transport.cancel(token) }\n\
\    public func listen(event: String,\n\
\        completion: [String : AnyObject] -> Void) -> " ⧺ protoName ⧺ ".CancellationToken \
\{ transport.listen(event, completion: completion) }\n\
\    public func cast(method: String, arguments: [String : AnyObject]) \
\{ transport.cast(method, arguments: arguments) }\n\
\    public init(transport: " ⧺ protoName ⧺ ") { self.transport = transport }\n\
\    public let transport: " ⧺ protoName ⧺ "\n\n" ⧺ methods ⧺ "}\n"

entDefs = "import Foundation\n\
\\n\
\// based on http://radex.io/swift/json/\n\
\\n\
\infix operator </ { associativity left }\n\
\infix operator </? { associativity left }\n\
\\n\
\func </ <A, B, C>(decoding: (A -> B -> C, [String: AnyObject])?, key: String) -> (B -> C, [String: AnyObject])? {\n\
\    if let (f, json) = decoding, x = json[key] as? A {\n\
\        return (f(x), json)\n\
\    } else {\n\
\        return nil\n\
\    }\n\
\}\n\
\\n\
\func </ <A, B>(decoding: (A -> B, [String: AnyObject])?, key: String) -> B? {\n\
\    if let (f, json) = decoding, x = json[key] as? A {\n\
\        return f(x)\n\
\    } else {\n\
\        return nil\n\
\    }\n\
\}\n\
\\n\
\func </? <A, B, C>(decoding: (A? -> B -> C, [String: AnyObject])?, key: String) -> (B -> C, [String: AnyObject])? {\n\
\    if let (f, json) = decoding {\n\
\        return (f(json[key] as? A), json)\n\
\    } else {\n\
\        return nil\n\
\    }\n\
\}\n\
\\n\
\func </? <A, B>(decoding: (A? -> B, [String: AnyObject])?, key: String) -> B? {\n\
\    if let (f, json) = decoding {\n\
\        return f(json[key] as? A)\n\
\    } else {\n\
\        return nil\n\
\    }\n\
\}\n\
\\n\
\public protocol JSONEncodable {\n\
\    var json: [String : AnyObject] { get }\n\
\    var Name: String { get }\n\
\}\n\
\public protocol JSONDecodable {\n\
\    init(json: [String : AnyObject])\n\
\}\n\
\extension Dictionary {\n\
\    var json: [String : AnyObject] { get {\n\
\        var d = [String : AnyObject](); for k in self.keys {\n\
\          let o = self[k]; if let o = o as? AnyObject { d[toString(k)] = o }\n\
\          else { d[k] = (o as! JSONEncodable).json }\n\
\        }\n\
\        return d\n\
\    }}\n\
\}\n\
\extension Array {\n\
\    var json: [String : AnyObject] { get {\n\
\        var d = [String : AnyObject](); for i in 0..<count {\n\
\          let o = self[i]; if let o = o as? AnyObject { d[toString(i)] = o }\n\
\          else { d[toString(i)] = (o as! JSONEncodable).json }\n\
\        }\n\
\        return d\n\
\    }}\n\
\}\n"

