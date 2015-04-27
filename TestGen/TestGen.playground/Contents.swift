//// http://j.mp/HsRPCGen

import TestGen

let json = ["age": 15, "name": "T",
            "creds": ["login":"user1", "pass":"123"],
            "services":["":[""]]] as JSON

let u = UserInfo(json)

println(u.creds?.login)

RPC.register("qwe123", username: "user1")
