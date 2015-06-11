{-# LANGUAGE UnicodeSyntax #-}
module Service where

import Network.HTTP.Server
import Network.HTTP.Server.Logger
import Codec.Binary.UTF8.String
import Data.List (intercalate)
import Data.List.Split (splitOn)

import Paths_burningbar (version)
import Data.Version (showVersion)

import Util
import Language
import Parse
import Swift
import Checker

srv = serverWith config process
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

toSwift = toJSON ∘ checkOrGen
  where toJSON (Right (e, i)) = "{ \"Entities\": \"" ⧺ escape e
                              ⧺ "\", \"Interface\": \"" ⧺ escape i
                              ⧺ "\", " ⧺ ver ⧺ "\"}"
        toJSON (Left err) = "{ \"error\": \"" ⧺ escape err ⧺ "\", " ⧺ ver ⧺ "\"}"
        checkOrGen rawSpec = let errors = checkSpec rawSpec in
                             if null errors then Right (gen rawSpec)
                                            else Left errors
        gen = translator (swift False "Transport" "Interface") ∘ parse
        ver = "\"version\": \"" ⧺ showVersion version

config = defaultConfig { srvLog = stdLogger, srvPort = 9604, srvHost = "0.0.0.0" }

-- | escapes quotes and newlines
-- >>> escape "\"\n"
-- "\\\"\\n"
escape = replace "\"" "\\\"" ∘ replace "\n" "\\n"

-- | relace substring
-- >>> replace "a" "b" "ab"
-- "bb"
replace old new = intercalate new ∘ splitOn old

