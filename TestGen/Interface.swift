// 📏🔥 Generated with http://j.mp/burnbar v0.5.12

import Foundation

public protocol Transport {
    typealias CancellationToken
    func call(method: String, arguments: [String: AnyObject],
              completion: [String: AnyObject] -> Void) -> CancellationToken
}
public class Interface <T: Transport> {

    public init(transport: T) { t = transport }

    public func ping(completion: Void -> Void) -> T.CancellationToken {
      return t.call("ping", arguments: [:]) {  _ in }
    }

    public func login(creds: Credentials, completion: UserInfo -> Void) -> T.CancellationToken {
      return t.call("user.login", arguments: ["creds": creds as! AnyObject]) { r in
        let v = UserInfo(r)
        completion(v)
      }
    }

    public func register(username: String, password: String, completion: Credentials -> Void) -> T.CancellationToken {
      return t.call("register", arguments: ["username": username as! AnyObject, "password": password as! AnyObject]) { r in
        let v = Credentials(r)
        completion(v)
      }
    }

    public func test(a1: Int, a2: NSNumber?, a3: Bool, completion: Void -> Void) -> T.CancellationToken {
      return t.call("test", arguments: ["a1": a1 as! AnyObject, "a2": (a2 ?? "null") as! AnyObject, "a3": a3 as! AnyObject]) {  _ in }
    }

    private let t: T
}
