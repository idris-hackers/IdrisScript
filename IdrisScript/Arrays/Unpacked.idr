module IdrisScript.Arrays.Unpacked

import IdrisScript

%access public

singleton : ToJS from to => from -> IO (JSValue JSArray)
singleton {from} {to} val = do
  arr <- mkForeign (FFun "[%0]" [FPtr] FPtr) (unpack (toJS {from}{to} val))
  return $ MkJSObject arr

push : ToJS from to => JSValue JSArray -> from -> IO ()
push {from} {to} arr val =
  mkForeign (
      FFun "%0.push(%1)" [FPtr, FPtr] FUnit
    ) (unpack arr) (unpack (toJS {from}{to} val))

insert : ToJS from to => Nat -> from -> JSValue JSArray -> IO (JSValue JSArray)
insert {from} {to} idx val arr = do
  mkForeign (
      FFun "%0[%1] = %2" [FPtr, FInt, FPtr] FUnit
    ) (unpack arr) (cast idx) (unpack (toJS {from}{to} val))
  return arr

indexOf : ToJS from to => from -> JSValue JSArray -> IO Int
indexOf {from} {to} val arr =
  mkForeign (
    FFun "%0.indexOf(%1)" [FPtr, FPtr] FInt
    ) (unpack arr) (unpack (toJS {from}{to} val))

unshift : ToJS from to => JSValue JSArray -> from -> IO Nat
unshift {from} {to} arr val = do
  res <- mkForeign (
      FFun "%0.unshift(%1)" [FPtr, FPtr] FInt
    ) (unpack arr) (unpack (toJS {from}{to} val))
  return $ cast {to=Nat} res
