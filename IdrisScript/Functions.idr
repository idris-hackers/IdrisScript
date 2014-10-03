module IdrisScript.Functions

import IdrisScript

%access public

Function : IO (JSValue JSFunction)
Function = do
  fun <- mkForeign (FFun "Function" [] FPtr)
  return $ MkJSFunction fun

apply : JSValue JSFunction -> JSValue JSArray -> IO (t ** JSValue t)
apply fun arg = do
  res <- mkForeign (
      FFun "%0.apply(this, %1)" [FPtr, FPtr] FPtr
    ) (unpack fun) (unpack arg)
  pack res

setProperty : String
           -> JSValue a
           -> JSValue JSFunction
           -> IO (JSValue JSFunction)
setProperty prop val fun = do
  mkForeign (
      FFun "%0[%1] = %2" [FPtr, FString, FPtr] FPtr
    ) (unpack fun) prop (unpack val)
  return fun

getProperty : String -> JSValue JSFunction -> IO (Maybe (t ** JSValue t))
getProperty prop fun = do
  elm <- mkForeign (FFun "%0[%1]" [FPtr, FString] FPtr) (unpack fun) prop
  case !(typeOf elm) of
       JSUndefined => return Nothing
       _           => return $ Just !(pack elm)

infixl 6 !!
(!!) : JSValue JSFunction -> String -> IO (Maybe (t : JSType ** JSValue t))
fun !! prop = getProperty prop fun

hasOwnProperty : String -> JSValue JSFunction -> IO Bool
hasOwnProperty prop fun = do
  res <- mkForeign (
      FFun "%0.hasOwnProperty(%1)" [FPtr, FString] FInt
    ) (unpack fun) prop
  return $ res == 1

name : JSValue JSFunction -> IO String
name fun = mkForeign (FFun "%0.name" [FPtr] FString) (unpack fun)
