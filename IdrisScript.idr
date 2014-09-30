module IdrisScript

%access public

data JSType = JSNumber
            | JSString
            | JSBoolean
            | JSFunction
            | JSNull
            | JSArray
            | JSObject
            | JSUndefined

instance Eq JSType where
  JSNumber      == JSNumber    = True
  JSString      == JSString    = True
  JSBoolean     == JSBoolean   = True
  JSFunction    == JSFunction  = True
  JSNull        == JSNull      = True
  JSArray       == JSArray     = True
  JSObject      == JSObject    = True
  JSUndefined   == JSUndefined = True
  _             == _           = False

data JSValue : JSType -> Type where
  MkJSNumber    : Ptr -> JSValue JSNumber
  MkJSString    : Ptr -> JSValue JSString
  MkJSBoolean   : Ptr -> JSValue JSBoolean
  MkJSFunction  : Ptr -> JSValue JSFunction
  MkJSNull      : Ptr -> JSValue JSNull
  MkJSArray     : Ptr -> JSValue JSArray
  MkJSObject    : Ptr -> JSValue JSObject
  MkJSUndefined : Ptr -> JSValue JSUndefined

typeOf : Ptr -> IO JSType
typeOf ptr = do
  res <- mkForeign (FFun checkType [FPtr] FInt) ptr
  case res of
       0 => return JSNumber
       1 => return JSString
       2 => return JSBoolean
       3 => return JSFunction
       4 => return JSNull
       5 => return JSArray
       6 => return JSObject
       _ => return JSUndefined
where
  checkType : String
  checkType =
    """(function(arg) {
         if (typeof arg == 'number')
           return 0;
         else if (typeof arg == 'string')
           return 1;
         else if (typeof arg == 'boolean')
           return 2;
         else if (typeof arg == 'function')
           return 3;
         else if (arg === null)
           return 4;
         else if (typeof arg == 'object' && arg.constructor == Array)
           return 5;
         else if (typeof arg == 'object')
           return 6;
         else
           return 7;
       })(%0)"""

isUndefined : Ptr -> IO Bool
isUndefined ptr = do
  ty <- typeOf ptr
  return $ ty == JSUndefined

class Convert from (to : JSType) where
  convert : from -> JSValue to

instance Convert String JSString where
  convert str = MkJSString (believe_me str)

instance Convert Int JSNumber where
  convert num = MkJSNumber (believe_me num)

instance Convert Float JSNumber where
  convert num = MkJSNumber (believe_me num)

instance Convert Bool JSBoolean where
  convert False = MkJSBoolean (believe_me 0)
  convert True  = MkJSBoolean (believe_me 1)

total
unpack : JSValue t -> Ptr
unpack (MkJSNumber ptr)    = ptr
unpack (MkJSString ptr)    = ptr
unpack (MkJSBoolean ptr)   = ptr
unpack (MkJSFunction ptr)  = ptr
unpack (MkJSNull ptr)      = ptr
unpack (MkJSArray  ptr)    = ptr
unpack (MkJSObject ptr)    = ptr
unpack (MkJSUndefined ptr) = ptr

pack : Ptr -> IO (t ** JSValue t)
pack ptr =
  case !(typeOf ptr) of
       JSNumber   => return (JSNumber    ** MkJSNumber    ptr)
       JSString   => return (JSString    ** MkJSString    ptr)
       JSBoolean  => return (JSBoolean   ** MkJSBoolean   ptr)
       JSFunction => return (JSFunction  ** MkJSFunction  ptr)
       JSNull     => return (JSNull      ** MkJSNull      ptr)
       JSArray    => return (JSArray     ** MkJSArray     ptr)
       JSObject   => return (JSObject    ** MkJSObject    ptr)
       _          => return (JSUndefined ** MkJSUndefined ptr)

log : Ptr -> IO ()
log ptr = mkForeign (FFun "console.log(%0)" [FPtr] FUnit) ptr
