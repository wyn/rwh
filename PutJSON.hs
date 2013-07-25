module PutJSON where

import Data.List (intercalate)
import SimpleJSON

renderJValue :: JValue -> String

renderJValue (JString s)   = show s
renderJValue (JNumber n)   = show n
renderJValue (JBool True)  = "true"
renderJValue (JBool False) = "false"
renderJValue JNull         = "null"

renderJValue (JObject o) = "{" ++ pairs o ++ "}"
    where pairs [] = ""
          pairs ps = intercalate ", " (map renderPair ps)
          renderPair (ky,vl) = "(" ++ show ky ++ ": " ++ renderJValue vl ++ ")"

renderJValue (JArray a) = "[" ++ renderObjs a ++ "]"
    where renderObjs [] = ""
          renderObjs ps = intercalate ", " (map renderJValue ps)

putJValue :: JValue -> IO ()
putJValue v = putStrLn (renderJValue v)