//// http://j.mp/JSON-Swift_hs

import TestGen

let u = UserInfo(json: ["age": 15, "name": "T",
                        "services":["s":["s"]]])
print(u.age)
