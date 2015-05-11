{-# LANGUAGE UnicodeSyntax, NamedFieldPuns #-}
import Network.HTTP.Server
import Network.HTTP.Server.Logger
import Network.HTTP.Server.HtmlForm as Form
import Network.URL as URL
import Codec.Binary.UTF8.String
import Control.Exception (try, SomeException)
import System.FilePath
import Unicode

main = serverWith config process
  where process _ url request = case rqMethod request of
              POST → (return ∘ sendText OK ∘ decodeString ∘ rqBody) request
              otherwise → return $ sendText BadRequest "Ｃ:。ミ"

sendText ∷ StatusCode → String → Response String
sendText s v = let txt = encodeString v in insertHeader HdrContentLength (show (length txt))
             $ insertHeader HdrContentEncoding "UTF-8"
             $ insertHeader HdrContentEncoding "text/plain"
             $ (respond s ∷ Response String) { rspBody = txt }

config = let srvLog = stdLogger
             srvPort = 9604
         in defaultConfig { srvLog, srvPort }

