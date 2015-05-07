// Generated with http://j.mp/burningbar

import Foundation

public struct UserInfo {
    public init(_ json: [String: AnyObject]) {
        age = json["age"] as! Int
        if let j = json["creds"] as? [String: AnyObject] { creds = Credentials(j)} else { creds = nil }
        name = json["name"] as? String
        photoURLs = json["photoURLs"] as? [String]
        services = json["services"] as! [String: [String]]
    }
    public let age: Int
    public let creds: Credentials?
    public let name: String?
    public let photoURLs: [String]?
    public let services: [String: [String]]
}

public struct Credentials {
    public init(_ json: [String: AnyObject]) {
        login = json["login"] as! String
        pass = json["pass"] as! String
    }
    public let login: String
    public let pass: String
}

