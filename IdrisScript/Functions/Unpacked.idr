module IdrisScript.Functions.Unpacked

import IdrisScript

%access public

setProperty : ToJS from to
           => String
           -> from
           -> JSValue JSFunction
           -> IO (JSValue JSFunction)
setProperty {from} {to} prop val fun = do
  mkForeign (
      FFun "%0[%1] = %2" [FPtr, FString, FPtr] FPtr
    ) (unpack fun) prop (unpack (toJS {from}{to} val))
  return fun
