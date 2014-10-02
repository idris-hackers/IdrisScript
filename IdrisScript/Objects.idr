module IdrisScript.Objects

import IdrisScript

Object : IO (JSValue JSFunction)
Object = do
  obj <- mkForeign (FFun "Object" [] FPtr)
  return $ MkJSFunction obj

empty : IO (JSValue (JSObject "Object"))
empty = do
  obj <- mkForeign (FFun "new Object()" [] FPtr)
  return $ MkJSObject obj

setProperty : String
           -> JSValue a
           -> JSValue (JSObject c)
           -> IO (JSValue (JSObject c))
setProperty prop val obj = do
  mkForeign (
      FFun "%0[%1] = %2" [FPtr, FString, FPtr] FPtr
    ) (unpack obj) prop (unpack val)
  return obj

getProperty : String -> JSValue (JSObject c) -> IO (Maybe (t ** JSValue t))
getProperty prop obj = do
  elm <- mkForeign (FFun "%0[%1]" [FPtr, FString] FPtr) (unpack obj) prop
  case !(typeOf elm) of
       JSUndefined => return Nothing
       _           => return $ Just !(pack elm)

infixl 6 !!
(!!) : JSValue (JSObject c) -> String -> IO (Maybe (t : JSType ** JSValue t))
obj !! prop = getProperty prop obj

hasOwnProperty : String -> JSValue (JSObject c) -> IO Bool
hasOwnProperty prop obj = do
  res <- mkForeign (
      FFun "%0.hasOwnProperty(%1)" [FPtr, FString] FInt
    ) (unpack obj) prop
  return $ res == 1
