{-# LANGUAGE UnicodeSyntax #-}
import Network.HTTP.Server
import Network.HTTP.Server.Logger
import Network.HTTP.Server.HtmlForm as Form
import Network.URL as URL
import Codec.Binary.UTF8.String
import Control.Exception (try, SomeException)
import System.FilePath

import Unicode
import Swift
import Parse

main = serverWith config process
  where process _ url request = case rqMethod request of
          POST → (return ∘ sendText OK ∘ decodeString ∘ rqBody) request
          otherwise → return $ sendText BadRequest "Ｃ:。ミ"

sendText ∷ StatusCode → String → Response String
sendText s v = headers reponse
  where reponse = (respond s ∷ Response String) { rspBody = toSwift text }
        headers = insertHeader HdrContentLength (show $ length text)
                ∘ insertHeader HdrContentEncoding "UTF-8"
                ∘ insertHeader HdrContentType "text/plain"
        text = encodeString v

toSwift = show ∘ translator (swift "Singularity" "Horizon") ∘ parse

config = defaultConfig { srvLog = stdLogger, srvPort = 9604 }

