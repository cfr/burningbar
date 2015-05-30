// 📏🔥 Generated with http://j.mp/burnbar v0.5.30

import Foundation

public func idTf<T>(a: T) -> T { return a }
public protocol Transport {
    typealias CancellationToken
    func cancel(token: CancellationToken)
    func cast(method: String, arguments: [String : AnyObject])
    func listen(event: String,
                completion: [String : AnyObject] -> Void) -> CancellationToken
    func call(method: String, arguments: [String : AnyObject],
              completion: [String : AnyObject] -> Void) -> CancellationToken
}

public class Interface <T: Transport> {
    public func cancel(token: T.CancellationToken) { transport.cancel(token) }
    public func listen(event: String,
        completion: [String : AnyObject] -> Void) -> T.CancellationToken { return transport.listen(event, completion: completion) }
    public func cast(method: String, arguments: [String : AnyObject]) { transport.cast(method, arguments: arguments) }
    public init(transport: T) { self.transport = transport }
    public let transport: T

    public func ping(tf: (Void? -> Void?) = idTf, completion: Void? -> Void) -> T.CancellationToken {
      return transport.call("ping", arguments: [:]) {  _ in }
    }
    public let ping: String = "ping"

    public func login(creds: Credentials, tf: (Void? -> Void?) = idTf, completion: Void? -> Void) -> T.CancellationToken {
      return transport.call("user.login", arguments: ["creds": creds.json]) {  _ in }
    }
    public let login: String = "login"

    public func register(username: String, password: String, tf: (Credentials? -> Credentials?) = idTf, completion: Credentials? -> Void) -> T.CancellationToken {
      return transport.call("register", arguments: ["username": username, "password": password]) { r in
        let v = Credentials(json: r)
        completion(v)
      }
    }
    public let register: String = "register"

}
