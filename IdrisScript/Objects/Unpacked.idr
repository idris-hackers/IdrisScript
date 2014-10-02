module IdrisScript.Objects.Unpacked

import IdrisScript

%access public

setProperty : ToJS from to
           => String
           -> from
           -> JSValue (JSObject c)
           -> IO (JSValue (JSObject c))
setProperty {from} {to} prop val obj = do
  mkForeign (
      FFun "%0[%1] = %2" [FPtr, FString, FPtr] FPtr
    ) (unpack obj) prop (unpack (toJS {from}{to} val))
  return obj
