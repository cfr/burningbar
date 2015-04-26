// Generated with http://j.mp/JSON-Swift_hs
public typealias JSON = Dictionary<String, Any>
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
// TBD
