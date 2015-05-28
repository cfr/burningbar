
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
    func listen(event: String, completion: [String : AnyObject] -> Void) { }
}

let printer = Printer()
let i = Interface(transport: printer)

i.register("u", password: "p", completion: { print($0) })
i.ping() { _ in }

let json = ["login":"l", "pass":"p"]

let c = Credentials(json: json)

print(c?.json)
