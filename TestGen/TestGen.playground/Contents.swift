
import Foundation
import TestGen

class Printer: Transport {
    typealias CancellationToken = Void
    func call(method: String, arguments: [String: AnyObject],
              completion: [String : AnyObject] -> Void)
               -> CancellationToken {
        println("Called \(method) with \(arguments)")
    }
}

let printer = Printer()
let i = Interface(transport: printer)

i.ping({ })

i.register("u", password: "pwd", completion: { print($0) })

let json = ["age": NSNumber(int:15), "friends": [], "name": "T",
            "creds": ["login":"user1", "pass":"123"],
            "services":["":[""]]] as [String : AnyObject]

//let u = UserInfo(json)

//println(u.creds?.serialized)

