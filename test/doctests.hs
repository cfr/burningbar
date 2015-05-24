import Control.Applicative
import System.Process
import Test.DocTest

test f = doctest (f:[ "-idist/build/", "-idist/build/autogen", "-optP-include"
                    , "-optPdist/build/autogen/cabal_macros.h", "src/Language.hs"
                    , "src/Parse.hs", "src/Swift.hs", "src/Static.hs", "src/Util.hs"])

main = mapM_ test ["src/BurningBar.hs", "src/Service.hs"]
