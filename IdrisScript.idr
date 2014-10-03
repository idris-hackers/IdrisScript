module IdrisScript

%access public

JSRaw : Type
JSRaw = Ptr

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
  MkJSNumber    : JSRaw -> JSValue JSNumber
  MkJSString    : JSRaw -> JSValue JSString
  MkJSBoolean   : JSRaw -> JSValue JSBoolean
  MkJSFunction  : JSRaw -> JSValue JSFunction
  MkJSNull      : JSRaw -> JSValue JSNull
  MkJSObject    : JSRaw -> JSValue (JSObject con)
  MkJSUndefined : JSRaw -> JSValue JSUndefined

JSArray : JSType
JSArray = JSObject "Array"

typeOf : JSRaw -> IO JSType
typeOf JSRaw = do
  res <- mkForeign (FFun checkType [FPtr] FInt) JSRaw
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
    mkForeign (FFun "%0.constructor.name" [FPtr] FString) JSRaw

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
unpack : JSValue t -> JSRaw
unpack (MkJSNumber JSRaw)    = JSRaw
unpack (MkJSString JSRaw)    = JSRaw
unpack (MkJSBoolean JSRaw)   = JSRaw
unpack (MkJSFunction JSRaw)  = JSRaw
unpack (MkJSNull JSRaw)      = JSRaw
unpack (MkJSObject JSRaw)    = JSRaw
unpack (MkJSUndefined JSRaw) = JSRaw

pack : JSRaw -> IO (t ** JSValue t)
pack JSRaw =
  case !(typeOf JSRaw) of
       JSNumber   => return (JSNumber    ** MkJSNumber    JSRaw)
       JSString   => return (JSString    ** MkJSString    JSRaw)
       JSBoolean  => return (JSBoolean   ** MkJSBoolean   JSRaw)
       JSFunction => return (JSFunction  ** MkJSFunction  JSRaw)
       JSNull     => return (JSNull      ** MkJSNull      JSRaw)
       JSObject c => return (JSObject c  ** MkJSObject    JSRaw)
       _          => return (JSUndefined ** MkJSUndefined JSRaw)

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

new : JSValue JSFunction -> JSValue JSArray -> IO (c ** JSValue (JSObject c))
new con args = do
  obj <- mkForeign (FFun """(function(con,args) {
                              function Con(con, args) {
                                return con.apply(this, args);
                              }
                              Con.prototype = con.prototype;
                              return new Con(con, args);
                            })(%0, %1)""" [FPtr, FPtr] FPtr
                   ) (unpack con) (unpack args)
  return $ (!(ctrName obj) ** MkJSObject obj)
where
  ctrName : JSRaw -> IO String
  ctrName JSRaw =
    mkForeign (FFun "%0.constructor.name" [FPtr] FString) JSRaw
