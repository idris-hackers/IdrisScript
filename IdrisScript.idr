module IdrisScript

%access public

JSRef : Type
JSRef = Ptr

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
  MkJSNumber    : JSRef -> JSValue JSNumber
  MkJSString    : JSRef -> JSValue JSString
  MkJSBoolean   : JSRef -> JSValue JSBoolean
  MkJSFunction  : JSRef -> JSValue JSFunction
  MkJSNull      : JSRef -> JSValue JSNull
  MkJSObject    : JSRef -> JSValue (JSObject con)
  MkJSUndefined : JSRef -> JSValue JSUndefined

JSArray : JSType
JSArray = JSObject "Array"

typeOf : JSRef -> IO JSType
typeOf JSRef = do
  res <- mkForeign (FFun checkType [FPtr] FInt) JSRef
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
    mkForeign (FFun "%0.constructor.name" [FPtr] FString) JSRef

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

||| Unpacks a JavaScript value
total
unpack : JSValue t -> JSRef
unpack (MkJSNumber JSRef)    = JSRef
unpack (MkJSString JSRef)    = JSRef
unpack (MkJSBoolean JSRef)   = JSRef
unpack (MkJSFunction JSRef)  = JSRef
unpack (MkJSNull JSRef)      = JSRef
unpack (MkJSObject JSRef)    = JSRef
unpack (MkJSUndefined JSRef) = JSRef

||| Packs up a JavaScript referenc into a JSValue
pack : JSRef -> IO (t ** JSValue t)
pack JSRef =
  case !(typeOf JSRef) of
       JSNumber   => return (JSNumber    ** MkJSNumber    JSRef)
       JSString   => return (JSString    ** MkJSString    JSRef)
       JSBoolean  => return (JSBoolean   ** MkJSBoolean   JSRef)
       JSFunction => return (JSFunction  ** MkJSFunction  JSRef)
       JSNull     => return (JSNull      ** MkJSNull      JSRef)
       JSObject c => return (JSObject c  ** MkJSObject    JSRef)
       _          => return (JSUndefined ** MkJSUndefined JSRef)

||| Log a value to console
log : JSValue t -> IO ()
log js = mkForeign (FFun "console.log(%0)" [FPtr] FUnit) (unpack js)

||| Check if a value is undefined
isUndefined : JSValue t -> IO Bool
isUndefined val = do
  ty <- typeOf (unpack val)
  return $ ty == JSUndefined

||| Check if a value is null
isNull : JSValue t -> IO Bool
isNull val = do
  ty <- typeOf (unpack val)
  return $ ty == JSNull

||| Create a new object with a constructor function
||| @ con constructor function
||| @ args constructor arguments
new : (con : JSValue JSFunction)
   -> (args : JSValue JSArray)
   -> IO (c ** JSValue (JSObject c))
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
  ctrName : JSRef -> IO String
  ctrName JSRef =
    mkForeign (FFun "%0.constructor.name" [FPtr] FString) JSRef
