// 📏🔥 Generated with http://j.mp/burnbar v0.5.14

import Foundation

public protocol Transport {
    typealias CancellationToken
    func cancel(token: CancellationToken)
    func call(method: String, arguments: [String : AnyObject],
              completion: [String : AnyObject] -> Void) -> CancellationToken
}
public class Interface <T: Transport> {

    public func cancel(token: T.CancellationToken) { transport.cancel(token) }
    public init(transport: T) { self.transport = transport }

    public func ping(completion: Void -> Void) -> T.CancellationToken {
      return transport.call("ping", arguments: [:]) {  _ in }
    }
    public let ping: String = "ping"
    public func login(creds: Credentials, completion: UserInfo -> Void) -> T.CancellationToken {
      return transport.call("user.login", arguments: ["creds": creds.asJSON]) { r in
        let v = UserInfo(r)
        completion(v)
      }
    }
    public let login: String = "login"
    public func register(username: String, password: String, completion: Credentials -> Void) -> T.CancellationToken {
      return transport.call("register", arguments: ["username": username, "password": password]) { r in
        let v = Credentials(r)
        completion(v)
      }
    }
    public let register: String = "register"
    public func test(a1: Int, a2: NSNumber?, a3: Bool, completion: Void -> Void) -> T.CancellationToken {
      return transport.call("test", arguments: ["a1": a1, "a2": (a2 ?? "null"), "a3": a3]) {  _ in }
    }
    public let test: String = "test"
    public let transport: T
}
