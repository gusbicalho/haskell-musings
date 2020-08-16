module RealWorldHaskell.Ch05.PutJSON where

import Data.List
import RealWorldHaskell.Ch05.SimpleJSON

renderJValue :: JValue -> String
renderJValue (JString s)     = show s
renderJValue (JNumber n)     = show n
renderJValue (JBool True)    = "true"
renderJValue (JBool False)   = "false"
renderJValue JNull           = "null"
renderJValue (JObject pairs) = "{" ++ renderAll pairs ++ "}"
  where renderAll = intercalate "," . map renderPair
        renderPair (key, value) = show key ++ ": " ++ renderJValue value
renderJValue (JArray values) = "[" ++ renderAll values ++ "]"
  where renderAll = intercalate "," . map renderJValue

putJValue :: JValue -> IO ()
putJValue v = putStrLn (renderJValue v)

-- >>> x = JString "as\nd"
-- >>> y = JNumber 42
-- >>> arr = JArray [x, y]
-- >>> obj = JObject [("x", x), ("y", y), ("arr", arr)]
-- >>> arr2 = JArray [x, y, obj]
-- >>> obj2 = JObject [("obj", obj), ("arr", arr), ("arr2", arr2)]
-- >>> obj2
-- >>> putJValue obj2
-- JObject [("obj",JObject [("x",JString "as\nd"),("y",JNumber 42.0),("arr",JArray [JString "as\nd",JNumber 42.0])]),("arr",JArray [JString "as\nd",JNumber 42.0]),("arr2",JArray [JString "as\nd",JNumber 42.0,JObject [("x",JString "as\nd"),("y",JNumber 42.0),("arr",JArray [JString "as\nd",JNumber 42.0])]])]
-- {"obj": {"x": "as\nd","y": 42.0,"arr": ["as\nd",42.0]},"arr": ["as\nd",42.0],"arr2": ["as\nd",42.0,{"x": "as\nd","y": 42.0,"arr": ["as\nd",42.0]}]}
--
