module IdrisScript.Objects.Unpacked

import IdrisScript

%access public

setProperty : ToJS from to
           => String
           -> from
           -> JSValue (JSObject c)
           -> JS_IO (JSValue (JSObject c))
setProperty {from} {to} prop val obj = do
  jscall "%0[%1] = %2" (Ptr -> String -> Ptr -> JS_IO Ptr)
         (unpack obj) prop (unpack (toJS {from}{to} val))
  return obj
