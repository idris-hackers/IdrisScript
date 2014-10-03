module IdrisScript.Objects

import IdrisScript

%access public

infixl 6 !!

Object : IO (JSValue JSFunction)
Object = do
  obj <- mkForeign (FFun "Object" [] FPtr)
  return $ MkJSFunction obj

||| Creates an empty JavaScript object.
empty : IO (JSValue (JSObject "Object"))
empty = do
  obj <- mkForeign (FFun "new Object()" [] FPtr)
  return $ MkJSObject obj

||| Sets the property `prop` to the value `val` for an object `obj`. Modifies
||| the original value.
setProperty : (prop : String)
           -> (val : JSValue a)
           -> (obj : JSValue (JSObject c))
           -> IO (JSValue (JSObject c))
setProperty prop val obj = do
  mkForeign (
      FFun "%0[%1] = %2" [FPtr, FString, FPtr] FPtr
    ) (unpack obj) prop (unpack val)
  return obj

||| Gets the property `prop` from an object `obj`.
getProperty : (prop : String)
           -> (obj : JSValue (JSObject c))
           -> IO (Maybe (t ** JSValue t))
getProperty prop obj = do
  elm <- mkForeign (FFun "%0[%1]" [FPtr, FString] FPtr) (unpack obj) prop
  case !(typeOf elm) of
       JSUndefined => return Nothing
       _           => return $ Just !(pack elm)

||| Gets the property `prop` from an object `obj`.
(!!) : (obj : JSValue (JSObject c))
    -> (prop : String)
    -> IO (Maybe (t : JSType ** JSValue t))
obj !! prop = getProperty prop obj

||| Checks if an object `obj` has the property `prop`.
hasOwnProperty : (prop : String)
              -> (obj : JSValue (JSObject c))
              -> IO Bool
hasOwnProperty prop obj = do
  res <- mkForeign (
      FFun "%0.hasOwnProperty(%1)" [FPtr, FString] FInt
    ) (unpack obj) prop
  return $ res == 1

||| Returns the keys of an object.
keys : JSValue (JSObject c) -> IO (JSValue JSArray)
keys obj = do
  keys <- mkForeign (FFun "Object.keys(%0)" [FPtr] FPtr) (unpack obj)
  return $ MkJSObject keys

||| Returns the constructor of an object.
constructor : JSValue (JSObject c) -> IO (JSValue JSFunction)
constructor obj = do
  con <- mkForeign (FFun "%0.constructor" [FPtr] FPtr) (unpack obj)
  return $ MkJSFunction con

||| Transforms a `Traversable` to an object.
toJSObject : (Traversable f, ToJS from to)
          => f (String, from)
          -> IO (JSValue (JSObject "Object"))
toJSObject {from} {to} xs = do
  obj <- empty
  traverse_ (\x =>
      setProperty (fst x) (toJS {from} {to} (snd x)) obj
    ) xs
  return obj
