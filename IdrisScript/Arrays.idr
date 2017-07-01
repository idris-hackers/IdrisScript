module IdrisScript.Arrays

import IdrisScript

%access public export

infixr 7 ++

Array : JS_IO (JSValue JSFunction)
Array = do
  arr <- jscall "Array" (JS_IO Ptr)
  pure $ MkJSFunction arr

||| Creates an empty JavaScript array.
empty : JS_IO (JSValue JSArray)
empty = do
  arr <- jscall "new Array()" (JS_IO Ptr)
  pure $ MkJSObject arr

||| Appends two JavaScript arrays.
(++) : JSValue JSArray -> JSValue JSArray -> JS_IO (JSValue JSArray)
arr ++ arr' = do
  res <- jscall "%0.concat(%1)" (Ptr -> Ptr -> JS_IO Ptr)
                (unpack arr) (unpack arr')
  pure $ MkJSObject res

||| Inserts a JavaScript value at position `idx`. Modifies the original value.
||| @ idx insert position.
insert : (idx : Nat) -> JSValue t -> JSValue JSArray -> JS_IO (JSValue JSArray)
insert idx val arr = do
  jscall "%0[%1] = %2" (Ptr -> Int -> Ptr -> JS_IO ())
    (unpack arr) (cast idx) (unpack val)
  pure arr

||| Returns the position of `val` in the array `arr`. Returns -1
||| if `val` is not present.
indexOf : (val : JSValue a) -> (arr : JSValue JSArray) -> JS_IO Int
indexOf val arr =
  jscall "%0.indexOf(%1)" (Ptr -> Ptr -> JS_IO Int)
    (unpack arr) (unpack val)

||| Flatten nested arrays.
join : JSValue JSArray -> JS_IO (JSValue JSArray)
join arr = do
  res <- jscall "%0.join()" (Ptr -> JS_IO Ptr) (unpack arr)
  pure $ MkJSObject res

||| Reverse an array. Modifies the original value.
reverse : JSValue JSArray -> JS_IO (JSValue JSArray)
reverse arr = do
  res <- jscall "%0.reverse()" (Ptr -> JS_IO Ptr) (unpack arr)
  pure $ MkJSObject res

||| Sort an array. Modifies the original value.
sort : JSValue JSArray -> JS_IO (JSValue JSArray)
sort arr = do
  res <- jscall "%0.sort()" (Ptr -> JS_IO Ptr) (unpack arr)
  pure $ MkJSObject res

||| Pushes the value `val` to the end of a array. Modifies original value.
push : JSValue JSArray -> (val : JSValue t) -> JS_IO ()
push arr val =
      jscall "%0.push(%1)" (Ptr -> Ptr -> JS_IO ())
              (unpack arr) (unpack val)

||| Pops a value from a the end of an array. Modifies the original value.
pop : JSValue JSArray -> JS_IO (Maybe (t : JSType ** JSValue t))
pop arr = do
  elm <- jscall "%0.pop()" (Ptr -> JS_IO Ptr) (unpack arr)
  case !(typeOf elm) of
       JSUndefined => pure Nothing
       _           => pure $ Just !(pack elm)

infixl 6 !!
(!!) : JSValue JSArray -> (idx : Nat) -> JS_IO (Maybe (t : JSType ** JSValue t))
arr !! idx = do
  elm <- jscall "%0[%1]" (Ptr -> Int -> JS_IO Ptr)
              (unpack arr) (cast idx)
  case !(typeOf elm) of
       JSUndefined => pure Nothing
       _           => pure $ Just !(pack elm)

||| Pops an element from the front of an array. Modifies the original value.
shift : JSValue JSArray -> JS_IO (Maybe (t ** JSValue t))
shift arr = do
  elm <- jscall "%0.shift()" (Ptr -> JS_IO Ptr) (unpack arr)
  case !(typeOf elm) of
       JSUndefined => pure Nothing
       _           => pure $ Just !(pack elm)

||| Pushes an element to the front of an array. Modifies the original value.
unshift : JSValue JSArray -> JSValue a -> JS_IO Nat
unshift arr val = do
  res <- jscall "%0.unshift(%1)" (Ptr -> Ptr -> JS_IO Int)
                 (unpack arr) (unpack val)
  pure $ cast {to=Nat} res

||| Return the head element of an array.
head : JSValue JSArray -> JS_IO (Maybe (t ** JSValue t))
head arr = do
  elm <- jscall "%0[0]" (Ptr -> JS_IO Ptr) (unpack arr)
  case !(typeOf elm) of
       JSUndefined => pure Nothing
       _           => pure $ Just !(pack elm)

||| Return the tail of an array. Returns an empty array in case
||| of an empty array.
tail : JSValue JSArray -> JS_IO (JSValue JSArray)
tail arr = do
  elm <- jscall
      """(function(arr) {
           return arr.slice(1, arr.length);
         })(%0)""" (Ptr -> JS_IO Ptr) (unpack arr)

  pure (MkJSObject elm)

||| Slices out an array form position `form` to `to`. Modifies original value.
slice : JSValue JSArray -> (from : Int) -> (to : Int) -> JS_IO (JSValue JSArray)
slice arr from to = do
  res <- jscall "%0.slice(%1,%2)" (Ptr -> Int -> Int -> JS_IO Ptr)
                (unpack arr) from to
  pure $ MkJSObject res

||| Creates an array with a single element.
singleton : JSValue t -> JS_IO (JSValue JSArray)
singleton val = do
  arr <- jscall "[%0]" (Ptr -> JS_IO Ptr) (unpack val)
  pure $ MkJSObject arr

||| Return the length of an array.
length : JSValue JSArray -> JS_IO Nat
length arr = do
  len <- jscall "%0.length" (Ptr -> JS_IO Int) (unpack arr)
  pure $ cast len

||| Runs an `JS_IO` action for every element of an array.
forEach : (JSRef -> JS_IO ()) -> JSValue JSArray -> JS_IO ()
forEach f arr =
    jscall "(function(arr,f){for(var i=0;i<arr.length;i++)f(arr[i])})(%0,%1)"
        (Ptr -> JsFn (Ptr -> JS_IO ()) -> JS_IO ())
         (unpack arr) (MkJsFn f)

||| Creates an arry from a `Traversable` data structure.
toJSArray : (Traversable f, ToJS from to) => f from -> JS_IO (JSValue JSArray)
toJSArray {from} {to} xs = do
  arr <- empty
  traverse_ (\x => arr `push` (toJS {from} {to} x)) xs
  pure arr
