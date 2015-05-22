module IdrisScript.Functions

import IdrisScript

%access public

infixl 6 !!

Function : JS_IO (JSValue JSFunction)
Function = do
  fun <- jscall "Function" (JS_IO Ptr)
  return $ MkJSFunction fun

||| Applys an array `args` as arguments to the function `fun`
apply : (fun : JSValue JSFunction)
     -> (args : JSValue JSArray)
     -> JS_IO (t ** JSValue t)
apply fun args = do
  res <- jscall "%0.apply(this, %1)" (Ptr -> Ptr -> JS_IO Ptr)
         (unpack fun) (unpack args)
  pack res

||| Sets the property `prop` to the value `val` for a function `fun`. Modifies
||| the original value.
setProperty : (prop : String)
           -> (val : JSValue a)
           -> (fun : JSValue JSFunction)
           -> JS_IO (JSValue JSFunction)
setProperty prop val fun = do
  jscall "%0[%1] = %2" (Ptr -> String -> Ptr -> JS_IO Ptr)
     (unpack fun) prop (unpack val)
  return fun

||| Gets the property `prop` from a function `fun`.
getProperty : (prop : String)
           -> (fun : JSValue JSFunction)
           -> JS_IO (Maybe (t ** JSValue t))
getProperty prop fun = do
  elm <- jscall "%0[%1]" (Ptr -> String -> JS_IO Ptr) (unpack fun) prop
  case !(typeOf elm) of
       JSUndefined => return Nothing
       _           => return $ Just !(pack elm)

||| Gets the property `prop` from a function `fun`.
(!!) : (fun : JSValue JSFunction)
    -> (prop : String)
    -> JS_IO (Maybe (t : JSType ** JSValue t))
fun !! prop = getProperty prop fun

||| Checks if a function `fun` has the property `prop`.
hasOwnProperty : (prop : String)
              -> (fun : JSValue JSFunction)
              -> JS_IO Bool
hasOwnProperty prop fun = do
  res <- jscall "%0.hasOwnProperty(%1)" (Ptr -> String -> JS_IO Int)
                (unpack fun) prop
  return $ res == 1

||| Returns the name of a function.
name : JSValue JSFunction -> JS_IO String
name fun = jscall "%0.name" (Ptr -> JS_IO String) (unpack fun)

||| Returns the constructor of a function.
constr : JSValue JSFunction -> JS_IO (JSValue JSFunction)
constr fun = do
  con <- jscall "%0.constructor" (Ptr -> JS_IO Ptr) (unpack fun)
  return $ MkJSFunction con
