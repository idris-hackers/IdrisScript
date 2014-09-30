module IdrisScript.Arrays

import IdrisScript

%access public

empty : IO (JSValue JSArray)
empty = do
  arr <- mkForeign (FFun "new Array()" [] FPtr)
  return $ MkJSArray arr

concat : JSValue JSArray -> JSValue JSArray -> IO (JSValue JSArray)
concat (MkJSArray arr) (MkJSArray arr') = do
  res <- mkForeign (FFun "%0.concat(%1)" [FPtr, FPtr] FPtr) arr arr'
  return $ MkJSArray res

reverse : JSValue JSArray -> IO (JSValue JSArray)
reverse (MkJSArray arr) = do
  res <- mkForeign (FFun "%0.reverse()" [FPtr] FPtr) arr
  return $ MkJSArray res

sort : JSValue JSArray -> IO (JSValue JSArray)
sort (MkJSArray arr) = do
  res <- mkForeign (FFun "%0.sort()" [FPtr] FPtr) arr
  return $ MkJSArray res

push : JSValue JSArray -> JSValue t -> IO ()
push (MkJSArray arr) val =
  mkForeign (FFun "%0.push(%1)" [FPtr, FPtr] FUnit) arr (unpack val)

pop : JSValue JSArray -> IO (Maybe (t : JSType ** JSValue t))
pop (MkJSArray arr) = do
  elm <- mkForeign (FFun "%0.pop()" [FPtr] FPtr) arr
  case !(typeOf elm) of
       JSUndefined => return Nothing
       _           => return $ Just !(pack elm)

infixl 6 !!
(!!) : JSValue JSArray -> Nat -> IO (Maybe (t : JSType ** JSValue t))
(MkJSArray arr) !! idx = do
  elm <- mkForeign (FFun "%0[%1]" [FPtr, FInt] FPtr) arr (
      cast {from=Nat} {to=Int} idx
    )
  case !(typeOf elm) of
       JSUndefined => return Nothing
       JSNull      => return Nothing
       _           => return $ Just !(pack elm)
