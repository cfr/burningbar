// Generated with http://j.mp/HsRPCGen

public typealias JSON = Dictionary<String, AnyObject>

public class RPC {
    public class func call(method: String, _ args: JSON) -> JSON {
        print("calling \(method) with \(args.description)")
        return [:]
    }
}

public struct UserInfo {
    public init(_ json: JSON) {
        age = json["age"] as! Int
        if let j = json["creds"] as? JSON { creds = Credentials(j) } else { creds = nil }
        name = json["name"] as? String
        photoURLs = json["photoURLs"] as? [String]
        services = json["services"] as! [String : [String]]
    }
    public let age: Int
    public let creds: Credentials?
    public let name: String?
    public let photoURLs: [String]?
    public let services: [String : [String]]
}

public struct Credentials {
    public init(_ json: JSON) {
        login = json["login"] as! String
        pass = json["pass"] as! String
    }
    public let login: String
    public let pass: String
}

public extension RPC {
  public class func register(password: String, username: String) -> Void {
      call("user.register", ["password": password ,"username": username])
  }
}


