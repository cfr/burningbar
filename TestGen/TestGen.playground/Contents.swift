
import TestGen

class Printer: Transport {
    typealias CancellationToken = Void
    func call(method: String, arguments: [String: Any],
              completion: [String : AnyObject] -> Void)
               -> CancellationToken {
        println("Called \(method) with \(arguments)")
    }
}

let json = ["age": 15, "name": "T",
            "creds": ["login":"user1", "pass":"123"],
            "services":["":[""]]] as [String : AnyObject]

let u = UserInfo(json)

println(u.creds?.login)

let printer = Printer()
let i = Interface(transport: printer)

i.ping({ })

i.register("user", password: "pwd", completion: { print($0) })
