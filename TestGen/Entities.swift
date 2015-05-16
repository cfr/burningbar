// 📏🔥 Generated with http://j.mp/burnbar v0.5.14

import Foundation

public struct Credentials : BBSerializable {
    public let asDictionary: [String : AnyObject]
    public init(_ json: [String : AnyObject]) {
        asDictionary = json
        login = json["login"] as? String
        pass = json["pass"] as? String
    }
    public static let Name = "Credentials"
    public let Name = "Credentials"
    public static let login = "login"
    public static let pass = "pass"

    public static func putLogin(login: String) -> ((inout [String : AnyObject]) -> [String : AnyObject]) {
        return { (inout d: [String : AnyObject]) in d["login"] = login; return d }
    }
    public static func putPass(pass: String) -> ((inout [String : AnyObject]) -> [String : AnyObject]) {
        return { (inout d: [String : AnyObject]) in d["pass"] = pass; return d }
    }
    public var login: String? = "user"
    public var pass: String?
}

public struct UserInfo : YourProto {
    public let asDictionary: [String : AnyObject]
    public init(_ json: [String : AnyObject]) {
        asDictionary = json
        friends = [String: UserInfo]()
        age = json["age"] as? NSNumber
        photoURLs = json["photoURLs"] as? [String]
        if let json = json["creds"] as? [String : AnyObject] {
          creds = Credentials(json)
        } else { creds = nil }
        map(json.keys) {(k: String) in self.friends[k] = UserInfo(json[k] as! [String : AnyObject])}
        name = json["name"] as? String
    }
    public static let Name = "UserInfo"
    public let Name = "UserInfo"
    public static let age = "age"
    public static let photoURLs = "photoURLs"
    public static let creds = "creds"
    public static let friends = "friends"
    public static let name = "name"

    public static func putAge(age: NSNumber) -> ((inout [String : AnyObject]) -> [String : AnyObject]) {
        return { (inout d: [String : AnyObject]) in d["age"] = age; return d }
    }
    public static func putPhotoURLs(photoURLs: [String]) -> ((inout [String : AnyObject]) -> [String : AnyObject]) {
        return { (inout d: [String : AnyObject]) in d["photoURLs"] = photoURLs; return d }
    }
    public static func putCreds(creds: Credentials) -> ((inout [String : AnyObject]) -> [String : AnyObject]) {
        return { (inout d: [String : AnyObject]) in d["creds"] = creds.asDictionary; return d }
    }
    public static func putFriends(friends: [String: UserInfo]) -> ((inout [String : AnyObject]) -> [String : AnyObject]) {
        return { (inout d: [String : AnyObject]) in d["friends"] = friends.asDictionary; return d }
    }
    public static func putName(name: String) -> ((inout [String : AnyObject]) -> [String : AnyObject]) {
        return { (inout d: [String : AnyObject]) in d["name"] = name; return d }
    }
    public var age: NSNumber?
    public var photoURLs: [String]?
    public var creds: Credentials?
    public var friends: [String: UserInfo]
    public var name: String?
}


public protocol BBSerializable {
    var asDictionary: [String : AnyObject] { get }
    var Name: String { get }
}
extension Dictionary {
    var asDictionary: [Key : AnyObject] { get {
        var d = [Key : AnyObject](); for k in self.keys {
          let o = self[k]; if let o: AnyObject = o as? AnyObject { d[k] = o }
          else { d[k] = (o as! BBSerializable).asDictionary }
        }
        return d
    }}
}