module IdrisScript.Arrays

import IdrisScript

%access public

infixr 7 ++

Array : IO (JSValue JSFunction)
Array = do
  arr <- mkForeign (FFun "Array" [] FPtr)
  return $ MkJSFunction arr

||| Creates an empty JavaScript array.
empty : IO (JSValue JSArray)
empty = do
  arr <- mkForeign (FFun "new Array()" [] FPtr)
  return $ MkJSObject arr

||| Appends two JavaScript arrays.
(++) : JSValue JSArray -> JSValue JSArray -> IO (JSValue JSArray)
arr ++ arr' = do
  res <- mkForeign (
      FFun "%0.concat(%1)" [FPtr, FPtr] FPtr
    ) (unpack arr) (unpack arr')
  return $ MkJSObject res

||| Inserts a JavaScript value at position `idx`. Modifies the original value.
||| @ idx insert position.
insert : (idx : Nat) -> JSValue t -> JSValue JSArray -> IO (JSValue JSArray)
insert idx val arr = do
  mkForeign (
      FFun "%0[%1] = %2" [FPtr, FInt, FPtr] FUnit
    ) (unpack arr) (cast idx) (unpack val)
  return arr

||| Returns the position of `val` in the array `arr`. Returns -1
||| if `val` is not present.
indexOf : (val : JSValue a) -> (arr : JSValue JSArray) -> IO Int
indexOf val arr =
  mkForeign (
      FFun "%0.indexOf(%1)" [FPtr, FPtr] FInt
    ) (unpack arr) (unpack val)

||| Flatten nested arrays.
join : JSValue JSArray -> IO (JSValue JSArray)
join arr = do
  res <- mkForeign (FFun "%0.join()" [FPtr] FPtr) (unpack arr)
  return $ MkJSObject res

||| Reverse an array. Modifies the original value.
reverse : JSValue JSArray -> IO (JSValue JSArray)
reverse arr = do
  res <- mkForeign (FFun "%0.reverse()" [FPtr] FPtr) (unpack arr)
  return $ MkJSObject res

||| Sort an array. Modifies the original value.
sort : JSValue JSArray -> IO (JSValue JSArray)
sort arr = do
  res <- mkForeign (FFun "%0.sort()" [FPtr] FPtr) (unpack arr)
  return $ MkJSObject res

||| Pushes the value `val` to the end of a array. Modifies original value.
push : JSValue JSArray -> (val : JSValue t) -> IO ()
push arr val =
  mkForeign (
      FFun "%0.push(%1)" [FPtr, FPtr] FUnit
    ) (unpack arr) (unpack val)

||| Pops a value from a the end of an array. Modifies the original value.
pop : JSValue JSArray -> IO (Maybe (t : JSType ** JSValue t))
pop arr = do
  elm <- mkForeign (FFun "%0.pop()" [FPtr] FPtr) (unpack arr)
  case !(typeOf elm) of
       JSUndefined => return Nothing
       _           => return $ Just !(pack elm)

infixl 6 !!
(!!) : JSValue JSArray -> (idx : Nat) -> IO (Maybe (t : JSType ** JSValue t))
arr !! idx = do
  elm <- mkForeign (
      FFun "%0[%1]" [FPtr, FInt] FPtr
    ) (unpack arr) (cast idx)
  case !(typeOf elm) of
       JSUndefined => return Nothing
       _           => return $ Just !(pack elm)

||| Pops an element from the front of an array. Modifies the original value.
shift : JSValue JSArray -> IO (Maybe (t ** JSValue t))
shift arr = do
  elm <- mkForeign (FFun "%0.shift()" [FPtr] FPtr) (unpack arr)
  case !(typeOf elm) of
       JSUndefined => return Nothing
       _           => return $ Just !(pack elm)

||| Pushes an element to the front of an array. Modifies the original value.
unshift : JSValue JSArray -> JSValue a -> IO Nat
unshift arr val = do
  res <- mkForeign (
      FFun "%0.unshift(%1)" [FPtr, FPtr] FInt
    ) (unpack arr) (unpack val)
  return $ cast {to=Nat} res

||| Return the head element of an array.
head : JSValue JSArray -> IO (Maybe (t ** JSValue t))
head arr = do
  elm <- mkForeign (FFun "%0[0]" [FPtr] FPtr) (unpack arr)
  case !(typeOf elm) of
       JSUndefined => return Nothing
       _           => return $ Just !(pack elm)

||| Return the tail of an array. Returns an empty array in case
||| of an empty array.
tail : JSValue JSArray -> IO (JSValue JSArray)
tail arr = do
  elm <- mkForeign (FFun
      """(function(arr) {
           return arr.slice(1, arr.length);
         })(%0)""" [FPtr] FPtr
    ) (unpack arr)

  return (MkJSObject elm)

||| Slices out an array form position `form` to `to`. Modifies original value.
slice : JSValue JSArray -> (from : Int) -> (to : Int) -> IO (JSValue JSArray)
slice arr from to = do
  res <- mkForeign (
      FFun "%0.slice(%1,%2)" [FPtr, FInt, FInt] FPtr
    ) (unpack arr) from to
  return $ MkJSObject res

||| Creates an array with a single element.
singleton : JSValue t -> IO (JSValue JSArray)
singleton val = do
  arr <- mkForeign (FFun "[%0]" [FPtr] FPtr) (unpack val)
  return $ MkJSObject arr

||| Return the length of an array.
length : JSValue JSArray -> IO Nat
length arr = do
  len <- mkForeign (FFun "%0.length" [FPtr] FInt) (unpack arr)
  return $ cast len

||| Runs an `IO` action for every element of an array.
forEach : (JSRef -> IO ()) -> JSValue JSArray -> IO ()
forEach f arr =
  mkForeign (
    FFun "(function(arr,f){for(var i=0;i<arr.length;i++)f(arr[i])})(%0,%1)"
        [FPtr, FFunction FPtr (FAny (IO ()))] FUnit
    ) (unpack arr) f

||| Creates an arry from a `Traversable` data structure.
toJSArray : (Traversable f, ToJS from to) => f from -> IO (JSValue JSArray)
toJSArray {from} {to} xs = do
  arr <- empty
  traverse_ (\x => arr `push` (toJS {from} {to} x)) xs
  return arr
