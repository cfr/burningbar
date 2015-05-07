// Generated with http://j.mp/burningbar

import Foundation

public class Interface {
    public init() { }
    public func call(method: String, _ args: [String: AnyObject], completion: [String: AnyObject] -> Void) -> [String: AnyObject] {
        print("calling \(method) with \(args.description)")
        return [:]
    }
}

public extension Interface {
  public func register(password: String, username: String, completion: (UserInfo -> Void)) -> Void {
      call("user.register", ["password": password ,"username": username]) {
        let v = UserInfo($0)
        completion(v)
      }
  }
}
