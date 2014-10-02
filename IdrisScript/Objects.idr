module IdrisScript.Objects

import IdrisScript

empty : IO (JSValue JSObject)
empty = do
  obj <- mkForeign (FFun "new Object()" [] FPtr)
  return $ MkJSObject obj

setProperty : String -> JSValue a -> JSValue JSObject -> IO (JSValue JSObject)
setProperty prop val obj = do
  mkForeign (
      FFun "%0[%1] = %2" [FPtr, FString, FPtr] FPtr
    ) (unpack obj) prop (unpack val)
  return obj

hasOwnProperty : String -> JSValue JSObject -> IO Bool
hasOwnProperty prop obj = do
  res <- mkForeign (
      FFun "%0.hasOwnProperty(%1)" [FPtr, FString] FInt
    ) (unpack obj) prop
  return $ res == 1
