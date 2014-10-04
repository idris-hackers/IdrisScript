module IdrisScript.Functions

import IdrisScript

%access public

infixl 6 !!

Function : IO (JSValue JSFunction)
Function = do
  fun <- mkForeign (FFun "Function" [] FPtr)
  return $ MkJSFunction fun

||| Applys an array `args` as arguments to the function `fun`
apply : (fun : JSValue JSFunction)
     -> (args : JSValue JSArray)
     -> IO (t ** JSValue t)
apply fun args = do
  res <- mkForeign (
      FFun "%0.apply(this, %1)" [FPtr, FPtr] FPtr
    ) (unpack fun) (unpack args)
  pack res

||| Sets the property `prop` to the value `val` for a function `fun`. Modifies
||| the original value.
setProperty : (prop : String)
           -> (val : JSValue a)
           -> (fun : JSValue JSFunction)
           -> IO (JSValue JSFunction)
setProperty prop val fun = do
  mkForeign (
      FFun "%0[%1] = %2" [FPtr, FString, FPtr] FPtr
    ) (unpack fun) prop (unpack val)
  return fun

||| Gets the property `prop` from a function `fun`.
getProperty : (prop : String)
           -> (fun : JSValue JSFunction)
           -> IO (Maybe (t ** JSValue t))
getProperty prop fun = do
  elm <- mkForeign (FFun "%0[%1]" [FPtr, FString] FPtr) (unpack fun) prop
  case !(typeOf elm) of
       JSUndefined => return Nothing
       _           => return $ Just !(pack elm)

||| Gets the property `prop` from a function `fun`.
(!!) : (fun : JSValue JSFunction)
    -> (prop : String)
    -> IO (Maybe (t : JSType ** JSValue t))
fun !! prop = getProperty prop fun

||| Checks if a function `fun` has the property `prop`.
hasOwnProperty : (prop : String)
              -> (fun : JSValue JSFunction)
              -> IO Bool
hasOwnProperty prop fun = do
  res <- mkForeign (
      FFun "%0.hasOwnProperty(%1)" [FPtr, FString] FInt
    ) (unpack fun) prop
  return $ res == 1

||| Returns the name of a function.
name : JSValue JSFunction -> IO String
name fun = mkForeign (FFun "%0.name" [FPtr] FString) (unpack fun)

||| Returns the constructor of a function.
constructor : JSValue JSFunction -> IO (JSValue JSFunction)
constructor fun = do
  con <- mkForeign (FFun "%0.constructor" [FPtr] FPtr) (unpack fun)
  return $ MkJSFunction con
