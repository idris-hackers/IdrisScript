module IdrisScript.Objects

import IdrisScript

%access public export

infixl 6 !!

Object : JS_IO (JSValue JSFunction)
Object = do
  obj <- jscall "Object" (JS_IO Ptr)
  return $ MkJSFunction obj

||| Creates an empty JavaScript object.
empty : JS_IO (JSValue (JSObject "Object"))
empty = do
  obj <- jscall "new Object()" (JS_IO Ptr)
  return $ MkJSObject obj

||| Sets the property `prop` to the value `val` for an object `obj`. Modifies
||| the original value.
setProperty : (prop : String)
           -> (val : JSValue a)
           -> (obj : JSValue (JSObject c))
           -> JS_IO (JSValue (JSObject c))
setProperty prop val obj = do
  jscall "%0[%1] = %2" (Ptr -> String -> Ptr -> JS_IO Ptr)
         (unpack obj) prop (unpack val)
  return obj

||| Gets the property `prop` from an object `obj`.
getProperty : (prop : String)
           -> (obj : JSValue (JSObject c))
           -> JS_IO (Maybe (t ** JSValue t))
getProperty prop obj = do
  elm <- jscall "%0[%1]" (Ptr -> String -> JS_IO Ptr) (unpack obj) prop
  case !(typeOf elm) of
       JSUndefined => return Nothing
       _           => return $ Just !(pack elm)

||| Gets the property `prop` from an object `obj`.
(!!) : (obj : JSValue (JSObject c))
    -> (prop : String)
    -> JS_IO (Maybe (t : JSType ** JSValue t))
obj !! prop = getProperty prop obj

||| Checks if an object `obj` has the property `prop`.
hasOwnProperty : (prop : String)
              -> (obj : JSValue (JSObject c))
              -> JS_IO Bool
hasOwnProperty prop obj = do
  res <- jscall "%0.hasOwnProperty(%1)" (Ptr -> String -> JS_IO Int)
                (unpack obj) prop
  return $ res == 1

||| Returns the keys of an object.
keys : JSValue (JSObject c) -> JS_IO (JSValue JSArray)
keys obj = do
  keys <- jscall "Object.keys(%0)" (Ptr -> JS_IO Ptr) (unpack obj)
  return $ MkJSObject keys

||| Returns the constructor of an object.
constr : JSValue (JSObject c) -> JS_IO (JSValue JSFunction)
constr obj = do
  con <- jscall "%0.constructor" (Ptr -> JS_IO Ptr) (unpack obj)
  return $ MkJSFunction con

||| Transforms a `Traversable` to an object.
toJSObject : (Traversable f, ToJS from to)
          => f (String, from)
          -> JS_IO (JSValue (JSObject "Object"))
toJSObject {from} {to} xs = do
  obj <- empty
  traverse_ (\x =>
      setProperty (fst x) (toJS {from} {to} (snd x)) obj
    ) xs
  return obj
