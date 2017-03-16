module IdrisScript.JSON

import IdrisScript

%access public export

||| Converts an object into a JSON string
stringfy : JSValue (JSObject c) -> JS_IO String
stringfy obj = jscall "JSON.stringfy(%0)" (Ptr -> JS_IO String) (unpack obj)

||| Parses a JSON string
parse : String -> JS_IO (Maybe (c ** JSValue (JSObject c)))
parse str = do
  res <- jscall "JSON.parse(%0)" (String -> JS_IO Ptr) str
  case !(pack res) of
       (JSObject "Object" ** obj) => pure $ Just ("Object" ** obj)
       (JSObject "Array"  ** obj) => pure $ Just ("Array" ** obj)
       _                          => pure Nothing
