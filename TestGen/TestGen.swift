// Generated with http://j.mp/HsRPCGen

public typealias JSON = Dictionary<String, Any>

public class RPC {
    public class func call(args: JSON, _ method: String) -> JSON {
        print("calling \(method) with \(args.description)")
        return [:]
    }
}

public struct UserInfo {
    public init(json: JSON) {
        age = json["age"] as! Int
        name = json["name"] as? String
        photoURLs = json["photoURLs"] as? [String]
        services = json["services"] as! [String  : [String]]
    }
    public let age: Int
    public let name: String?
    public let photoURLs: [String]?
    public let services: [String  : [String]]
}

public extension RPC {
  public class func tbd() -> Void {
    call([:], "user.tbd")
  }
}


