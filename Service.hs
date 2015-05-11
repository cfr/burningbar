{-# LANGUAGE UnicodeSyntax #-}
import Network.HTTP.Server
import Network.HTTP.Server.Logger
import Network.HTTP.Server.HtmlForm as Form
import Network.URL as URL
import Codec.Binary.UTF8.String
import Control.Exception (try, SomeException)
import System.FilePath
import Data.List (intercalate, break)
import Control.Arrow (second)

import Unicode
import Swift
import Parse

-- available at http://cfr.pw/burnbar
-- https://github.com/cfr/cfr.github.io/blob/master/burnbar.html
-- $ http localhost:9604 < spec.burnbar
main = serverWith config process
  where process _ url request = case rqMethod request of
          POST → (return ∘ sendJSON OK ∘ toSwift ∘ decodeString ∘ rqBody) request
          otherwise → return $ sendJSON BadRequest "[\"Ｃ:。ミ\"]"

sendJSON ∷ StatusCode → String → Response String
sendJSON s v = headers reponse
  where reponse = (respond s ∷ Response String) { rspBody = text }
        headers = insertHeader HdrContentLength (show $ length text)
                ∘ insertHeader HdrContentEncoding "UTF-8"
                ∘ insertHeader HdrContentType "application/json"
                ∘ insertHeader (HdrCustom "Access-Control-Allow-Origin") "*"
        text = encodeString v

toSwift = toJSON ∘ translator (swift "Singularity" "Horizon") ∘ parse
  where toJSON (e, i) = "{ \"Entities\": \"" ⧺ escape e
                        ⧺ "\", \"Interface\": \"" ⧺ escape i
                        ⧺ "\"}"
        escape = replace '\"' "\\\"" ∘ replace '\n' "\\n"

config = defaultConfig { srvLog = stdLogger, srvPort = 9604, srvHost = "0.0.0.0" }

replace old new = intercalate new ∘ split old
split _ [] = []
split d s = let (l, r) = break (≡ d) s in l : split d (drop 1 r)

