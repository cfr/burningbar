//// http://j.mp/HsRPCGen

import TestGen

let u = UserInfo(json: ["age": 15, "name": "T",
                        "services":["s":["s"]]])
print(u.age)

RPC.tbd()
