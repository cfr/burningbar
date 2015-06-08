
import Foundation
import TestGen

class Printer: Transport {
    typealias CancellationToken = Void
    func call(method: String, arguments: [String: AnyObject],
              completion: [String : AnyObject] -> Void)
               -> CancellationToken {
        println("Called \(method) with \(arguments)")
    }
    func cancel(token: CancellationToken) { }
    func cast(method: String, arguments: [String : AnyObject]) { }
    func listen(event: String, listener: [String : AnyObject] -> Void) { }
}

let printer = Printer()
let i = Interface(transport: printer)

i.register("u", password: "p", completion: { print($0) })
i.ping() { _ in }

let cj = ["login":"l", "pass":"p"]
let phpArr = [1 as AnyObject, "b" as AnyObject]
let fj: [String: AnyObject] = ["photoURLs": phpArr, "friends": [:], "name": "A",
                               "birth": "1986-03-25T11:30:00+04:00"]
let user = User(json: ["photoURLs": [], "creds": cj, "friends": ["f": fj], "name": "B"])
user == User(json: fj)
print(user)
if let b = user?.friends.values.array.last?.birth { print(b) }

// TODO: overloading mapping operator example
