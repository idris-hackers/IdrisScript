module IdrisScript.RegExps

import IdrisScript

%access public export

RegExp : JS_IO (JSValue JSFunction)
RegExp = do
  regex <- jscall "RegExp" (JS_IO Ptr)
  pure $ MkJSFunction regex

data RegExpFlags = Global
                 | IgnoreCase
                 | Multiline

implementation Eq RegExpFlags where
  Global     == Global     = True
  IgnoreCase == IgnoreCase = True
  Multiline  == Multiline  = True
  _          == _          = False

JSRegExp : JSType
JSRegExp = JSObject "RegExp"

||| Creates a new RegExp with a list of flags.
newRegExp : String -> List RegExpFlags -> JS_IO (JSValue JSRegExp)
newRegExp patt flags = do
  regex <- jscall "new RegExp(%0, %1)" (String -> String -> JS_IO Ptr)
                  patt (mkFlags . nub $ flags)
  pure $ MkJSObject regex
where
  mkFlags : List RegExpFlags -> String
  mkFlags (Global     :: fs) = "g" ++ mkFlags fs
  mkFlags (IgnoreCase :: fs) = "i" ++ mkFlags fs
  mkFlags (Multiline  :: fs) = "m" ++ mkFlags fs
  mkFlags []                 = ""

||| Uses a RegExp `regex` on the string `str`.
match : (str : String)
     -> (regex : JSValue JSRegExp)
     -> JS_IO (JSValue JSArray)
match str regex = do
  res <- jscall "%0.match(%1)" (String -> Ptr -> JS_IO Ptr)
                str (unpack regex)
  pure $ MkJSObject res

||| Replaces matches of `regex` with `rpl` in the string `str`. Modifies
||| the original value.
replace : (str : String)
       -> (regex : JSValue JSRegExp)
       -> (rpl : String)
       -> JS_IO String
replace str regex rpl =
      jscall "%0.replace(%1, %2)" (String -> Ptr -> String -> JS_IO String)
             str (unpack regex) rpl

||| Splits an array `str` at the occurences of `regex`.
split : (str : String)
     -> (regex : JSValue JSRegExp)
     -> JS_IO (JSValue JSArray)
split str regex = do
  res <- jscall "%0.split(%1)" (String -> Ptr -> JS_IO Ptr)
                str (unpack regex)
  pure $ MkJSObject res
