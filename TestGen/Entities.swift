// Generated with http://j.mp/burnbar v0.5.10

import Foundation

public struct Credentials {
    public init(_ json: [String: AnyObject]) {
        login = json["login"] as? String
        pass = json["pass"] as? String
    }
    public var serialized: [String: Any] { get {
        return ["login": login as Any, "pass": pass as Any] } }
    public var login: String?
    public var pass: String?
}

public struct UserInfo {
    public init(_ json: [String: AnyObject]) {
        age = json["age"] as? NSNumber
        photoURLs = json["photoURLs"] as? [String]
        if let json = json["creds"] as? [String: AnyObject] {
          creds = Credentials(json)
        } else { creds = nil }
        services = json["services"] as! [String: [String]]
        name = json["name"] as? String
    }
    public var serialized: [String: Any] { get {
        return ["age": age as Any, "photoURLs": photoURLs as Any, "creds": creds as Any, "services": services as Any, "name": name as Any] } }
    public var age: NSNumber?
    public var photoURLs: [String]?
    public var creds: Credentials?
    public var services: [String: [String]]
    public var name: String?
}

