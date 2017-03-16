module IdrisScript

%access public export

JSRef : Type
JSRef = Ptr

%inline
jscall : (fname : String) -> (ty : Type) ->
          {auto fty : FTy FFI_JS [] ty} -> ty
jscall fname ty = foreign FFI_JS fname ty

data JSType = JSNumber
            | JSString
            | JSBoolean
            | JSFunction
            | JSNull
            | JSObject String
            | JSUndefined

implementation Eq JSType where
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

typeOf : JSRef -> JS_IO JSType
typeOf JSRef = do
  res <- jscall checkType (Ptr -> JS_IO Int) JSRef
  case res of
       0 => pure JSNumber
       1 => pure JSString
       2 => pure JSBoolean
       3 => pure JSFunction
       4 => pure JSUndefined
       5 => pure (JSObject !ctrName)
       _ => pure JSNull
where
  ctrName : JS_IO String
  ctrName =
    jscall "%0.constructor.name" (Ptr -> JS_IO String) JSRef

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

interface ToJS from (to : JSType) where
  toJS : from -> JSValue to

implementation ToJS String JSString where
  toJS str = MkJSString (believe_me str)

implementation ToJS Int JSNumber where
  toJS num = MkJSNumber (believe_me num)

implementation ToJS Double JSNumber where
  toJS num = MkJSNumber (believe_me num)

implementation ToJS Bool JSBoolean where
  toJS False = MkJSBoolean (believe_me 0)
  toJS True  = MkJSBoolean (believe_me 1)

interface FromJS (from : JSType) to where
  fromJS : JSValue from -> to

implementation FromJS JSString String where
  fromJS (MkJSString str) = believe_me str

implementation FromJS JSNumber Int where
  fromJS (MkJSNumber num) = cast {from=Double} {to=Int} (believe_me num)

implementation FromJS JSNumber Double where
  fromJS (MkJSNumber num) = believe_me num

implementation FromJS JSBoolean Bool where
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
pack : JSRef -> JS_IO (t ** JSValue t)
pack JSRef =
  case !(typeOf JSRef) of
       JSNumber   => pure (JSNumber    ** MkJSNumber    JSRef)
       JSString   => pure (JSString    ** MkJSString    JSRef)
       JSBoolean  => pure (JSBoolean   ** MkJSBoolean   JSRef)
       JSFunction => pure (JSFunction  ** MkJSFunction  JSRef)
       JSNull     => pure (JSNull      ** MkJSNull      JSRef)
       JSObject c => pure (JSObject c  ** MkJSObject    JSRef)
       _          => pure (JSUndefined ** MkJSUndefined JSRef)

||| Log a value to console
log : JSValue t -> JS_IO ()
log js = jscall "console.log(%0)" (Ptr -> JS_IO ()) (unpack js)

||| Check if a value is undefined
isUndefined : JSValue t -> JS_IO Bool
isUndefined val = do
  ty <- typeOf (unpack val)
  pure $ ty == JSUndefined

||| Check if a value is null
isNull : JSValue t -> JS_IO Bool
isNull val = do
  ty <- typeOf (unpack val)
  pure $ ty == JSNull

||| Create a new object with a constructor function
||| @ con constructor function
||| @ args constructor arguments
new : (con : JSValue JSFunction)
   -> (args : JSValue JSArray)
   -> JS_IO (c ** JSValue (JSObject c))
new con args = do
  obj <- jscall  """(function(con,args) {
                          function Con(con, args) {
                            return con.apply(this, args);
                          }
                          Con.prototype = con.prototype;
                          return new Con(con, args);
                        })(%0, %1)""" (Ptr -> Ptr -> JS_IO Ptr)
                   (unpack con) (unpack args)
  pure $ (!(ctrName obj) ** MkJSObject obj)
where
  ctrName : JSRef -> JS_IO String
  ctrName JSRef =
    jscall "%0.constructor.name" (Ptr -> JS_IO String) JSRef
