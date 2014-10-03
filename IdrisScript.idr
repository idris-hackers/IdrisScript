module IdrisScript

%access public

data JSType = JSNumber
            | JSString
            | JSBoolean
            | JSFunction
            | JSNull
            | JSObject String
            | JSUndefined

instance Eq JSType where
  JSNumber      == JSNumber      = True
  JSString      == JSString      = True
  JSBoolean     == JSBoolean     = True
  JSFunction    == JSFunction    = True
  JSNull        == JSNull        = True
  (JSObject c)  == (JSObject c') = c == c'
  JSUndefined   == JSUndefined   = True
  _             == _             = False

data JSValue : JSType -> Type where
  MkJSNumber    : Ptr -> JSValue JSNumber
  MkJSString    : Ptr -> JSValue JSString
  MkJSBoolean   : Ptr -> JSValue JSBoolean
  MkJSFunction  : Ptr -> JSValue JSFunction
  MkJSNull      : Ptr -> JSValue JSNull
  MkJSObject    : Ptr -> JSValue (JSObject con)
  MkJSUndefined : Ptr -> JSValue JSUndefined

JSArray : JSType
JSArray = JSObject "Array"

typeOf : Ptr -> IO JSType
typeOf ptr = do
  res <- mkForeign (FFun checkType [FPtr] FInt) ptr
  case res of
       0 => return JSNumber
       1 => return JSString
       2 => return JSBoolean
       3 => return JSFunction
       4 => return JSUndefined
       5 => return (JSObject !ctrName)
       _ => return JSNull
where
  ctrName : IO String
  ctrName =
    mkForeign (FFun "%0.constructor.name" [FPtr] FString) ptr

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
         else if (typeof arg == 'undefined')
           return 4;
         else if (typeof arg == 'object')
           return 5;
         else
           return 6;
       })(%0)"""

class ToJS from (to : JSType) where
  toJS : from -> JSValue to

instance ToJS String JSString where
  toJS str = MkJSString (believe_me str)

instance ToJS Int JSNumber where
  toJS num = MkJSNumber (believe_me num)

instance ToJS Float JSNumber where
  toJS num = MkJSNumber (believe_me num)

instance ToJS Bool JSBoolean where
  toJS False = MkJSBoolean (believe_me 0)
  toJS True  = MkJSBoolean (believe_me 1)

class FromJS (from : JSType) to where
  fromJS : JSValue from -> to

instance FromJS JSString String where
  fromJS (MkJSString str) = believe_me str

instance FromJS JSNumber Int where
  fromJS (MkJSNumber num) = cast {from=Float} {to=Int} (believe_me num)

instance FromJS JSNumber Float where
  fromJS (MkJSNumber num) = believe_me num

instance FromJS JSBoolean Bool where
  fromJS (MkJSBoolean b) = check (believe_me b)
    where
      check : Int -> Bool
      check b = b >= 1

total
unpack : JSValue t -> Ptr
unpack (MkJSNumber ptr)    = ptr
unpack (MkJSString ptr)    = ptr
unpack (MkJSBoolean ptr)   = ptr
unpack (MkJSFunction ptr)  = ptr
unpack (MkJSNull ptr)      = ptr
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
       JSObject c => return (JSObject c  ** MkJSObject    ptr)
       _          => return (JSUndefined ** MkJSUndefined ptr)

log : JSValue t -> IO ()
log js = mkForeign (FFun "console.log(%0)" [FPtr] FUnit) (unpack js)

isUndefined : JSValue t -> IO Bool
isUndefined val = do
  ty <- typeOf (unpack val)
  return $ ty == JSUndefined

isNull : JSValue t -> IO Bool
isNull val = do
  ty <- typeOf (unpack val)
  return $ ty == JSNull
