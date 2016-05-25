module IdrisScript.Arrays.Unpacked

import IdrisScript

%access public export

singleton : ToJS from to => from -> JS_IO (JSValue JSArray)
singleton {from} {to} val = do
  arr <- jscall "[%0]" (Ptr -> JS_IO Ptr) (unpack (toJS {from}{to} val))
  return $ MkJSObject arr

push : ToJS from to => JSValue JSArray -> from -> JS_IO ()
push {from} {to} arr val =
      jscall "%0.push(%1)" (Ptr -> Ptr -> JS_IO ())
         (unpack arr) (unpack (toJS {from}{to} val))

insert : ToJS from to => Nat -> from -> JSValue JSArray -> JS_IO (JSValue JSArray)
insert {from} {to} idx val arr = do
  jscall "%0[%1] = %2" (Ptr -> Int -> Ptr -> JS_IO ())
         (unpack arr) (cast idx) (unpack (toJS {from}{to} val))
  return arr

indexOf : ToJS from to => from -> JSValue JSArray -> JS_IO Int
indexOf {from} {to} val arr =
    jscall "%0.indexOf(%1)" (Ptr -> Ptr -> JS_IO Int)
           (unpack arr) (unpack (toJS {from}{to} val))

unshift : ToJS from to => JSValue JSArray -> from -> JS_IO Nat
unshift {from} {to} arr val = do
  res <- jscall "%0.unshift(%1)" (Ptr -> Ptr -> JS_IO Int)
                (unpack arr) (unpack (toJS {from}{to} val))
  return $ cast {to=Nat} res
