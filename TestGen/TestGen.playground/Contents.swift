
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
}

let printer = Printer()
let i = Interface(transport: printer)

i.ping({ })

i.register("u", password: "p", completion: { print($0) })

let json = ["login":"l", "pass":"p"]

let c = Credentials(json)

print(c.asDictionary)
