module IdrisScript.JSON

import IdrisScript

%access public

stringfy : JSValue (JSObject c) -> IO String
stringfy obj = mkForeign (FFun "JSON.stringfy(%0)" [FPtr] FString) (unpack obj)

parse : String -> IO (Maybe (c ** JSValue (JSObject c)))
parse str = do
  res <- mkForeign (FFun "JSON.parse(%0)" [FString] FPtr) str
  case !(pack res) of
       (JSObject "Object" ** obj) => return $ Just ("Object" ** obj)
       (JSObject "Array"  ** obj) => return $ Just ("Array" ** obj)
       _                          => return Nothing
