//// http://j.mp/HsRPCGen

import TestGen

let u = UserInfo(["age": 15, "name": "T",
                  "creds": ["login":"a", "pass":""],
                  "services":["":[""]]])
println(u.creds?.login)

let c = Credentials(["login":"a", "pass":""])
c.login

RPC.tbd()
