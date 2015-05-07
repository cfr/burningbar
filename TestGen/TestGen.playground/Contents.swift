
import TestGen

let json = ["age": 15, "name": "T",
            "creds": ["login":"user1", "pass":"123"],
            "services":["":[""]]] as [String : AnyObject]

let u = UserInfo(json)

println(u.creds?.login)

let i = Interface()

i.register("qwe123", username: "user1") { _ in }
