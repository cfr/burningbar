-- $ brew install ghc cabal-install
-- $ cabal install json
-- $ ghc --make JSON→Swift.hs
-- $ ./JSON→Swift <file.json

module Main where

import qualified Text.JSON (decode)
import Text.JSON hiding (decode)
import Control.Monad.State

decode :: String -> JSValue
decode = either error id . resultToEither . Text.JSON.decode

toSwift :: JSValue -> State JSValue String
toSwift = return . show

main = interact $ \json -> evalState (toSwift $ decode json) JSNull

