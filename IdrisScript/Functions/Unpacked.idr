module IdrisScript.Functions.Unpacked

import IdrisScript

%access public export

setProperty : ToJS from to
           => String
           -> from
           -> JSValue JSFunction
           -> JS_IO (JSValue JSFunction)
setProperty {from} {to} prop val fun = do
  jscall "%0[%1] = %2" (Ptr -> String -> Ptr -> JS_IO Ptr)
         (unpack fun) prop (unpack (toJS {from}{to} val))
  return fun
