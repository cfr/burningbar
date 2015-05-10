// Generated with http://j.mp/burnbar v0.5.10

import Foundation

public protocol Transport {
    typealias CancellationToken
    func call(method: String, arguments: [String: Any],
              completion: [String: AnyObject] -> Void) -> CancellationToken
}
public class Interface <T: Transport> {

    public init(transport: T) { t = transport }

    public func ping(completion: Void -> Void) -> T.CancellationToken {
      return t.call("ping", arguments: [:]) {  _ in }
    }

    public func login(creds: Credentials, completion: UserInfo -> Void) -> T.CancellationToken {
      return t.call("user.login", arguments: ["creds": creds as Any]) { r in
        let v = UserInfo(r)
        completion(v)
      }
    }

    public func register(username: String, password: String, completion: Credentials -> Void) -> T.CancellationToken {
      return t.call("register", arguments: ["username": username as Any, "password": password as Any]) { r in
        let v = Credentials(r)
        completion(v)
      }
    }

    public func test(a1: Int, a2: NSNumber?, a3: Bool, completion: Void -> Void) -> T.CancellationToken {
      return t.call("test", arguments: ["a1": a1 as Any, "a2": a2 as Any, "a3": a3 as Any]) {  _ in }
    }

    private let t: T
}
