// 📏🔥 Generated with http://j.mp/burnbar v0.5.12

import Foundation

public struct Credentials : BBSerializable {
    public let asDictionary: [String: AnyObject]
    public init(_ json: [String: AnyObject]) {
        asDictionary = json
        login = json["login"] as? String
        pass = json["pass"] as? String
    }
    public static let Name = "Credentials"
    public static let login = "login"
    public static let pass = "pass"

    public static func putLogin(login: String) -> ((inout [String: AnyObject]) -> [String: AnyObject]) {
        return { (inout d: [String: AnyObject]) in d["login"] = login as? AnyObject; return d }
    }
    public static func putPass(pass: String) -> ((inout [String: AnyObject]) -> [String: AnyObject]) {
        return { (inout d: [String: AnyObject]) in d["pass"] = pass as? AnyObject; return d }
    }
    public var login: String?
    public var pass: String?
}

public struct UserInfo : BBSerializable {
    public let asDictionary: [String: AnyObject]
    public init(_ json: [String: AnyObject]) {
        asDictionary = json
        friends = [String: UserInfo]()
        age = json["age"] as? NSNumber
        photoURLs = json["photoURLs"] as? [String]
        if let json = json["creds"] as? [String: AnyObject] {
          creds = Credentials(json)
        } else { creds = nil }
        map(json.keys) {(k: String) in self.friends[k] = UserInfo(json[k] as! [String: AnyObject])}
        name = json["name"] as? String
    }
    public static let Name = "UserInfo"
    public static let age = "age"
    public static let photoURLs = "photoURLs"
    public static let creds = "creds"
    public static let friends = "friends"
    public static let name = "name"

    public static func putAge(age: NSNumber) -> ((inout [String: AnyObject]) -> [String: AnyObject]) {
        return { (inout d: [String: AnyObject]) in d["age"] = age as? AnyObject; return d }
    }
    public static func putPhotoURLs(photoURLs: [String]) -> ((inout [String: AnyObject]) -> [String: AnyObject]) {
        return { (inout d: [String: AnyObject]) in d["photoURLs"] = photoURLs as? AnyObject; return d }
    }
    public static func putCreds(creds: Credentials) -> ((inout [String: AnyObject]) -> [String: AnyObject]) {
        return { (inout d: [String: AnyObject]) in d["creds"] = creds as? AnyObject; return d }
    }
    public static func putFriends(friends: [String: UserInfo]) -> ((inout [String: AnyObject]) -> [String: AnyObject]) {
        return { (inout d: [String: AnyObject]) in d["friends"] = friends as? AnyObject; return d }
    }
    public static func putName(name: String) -> ((inout [String: AnyObject]) -> [String: AnyObject]) {
        return { (inout d: [String: AnyObject]) in d["name"] = name as? AnyObject; return d }
    }
    public var age: NSNumber?
    public var photoURLs: [String]?
    public var creds: Credentials?
    public var friends: [String: UserInfo]
    public var name: String?
}


public protocol BBSerializable {
    var asDictionary: [String: AnyObject] { get }
    static var Name: String { get }
}