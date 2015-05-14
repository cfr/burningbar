import Control.Applicative
import System.Process
import Test.DocTest

opts = ([ "-idist/build/", "-idist/build/autogen", "-optP-include"
        , "-optPdist/build/autogen/cabal_macros.h", "src/Language.hs"
        , "src/Parse.hs", "src/Swift.hs", "src/Unicode.hs"] ++)

main = do doctest $ opts ["src/BurningBar.hs"]
          doctest $ opts ["src/Service.hs"]
