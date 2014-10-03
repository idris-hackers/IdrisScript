module IdrisScript.RegExps

import IdrisScript

%access public

RegExp : IO (JSValue JSFunction)
RegExp = do
  regex <- mkForeign (FFun "RegExp" [] FPtr)
  return $ MkJSFunction regex

data RegExpFlags = Global
                 | IgnoreCase
                 | Multiline

instance Eq RegExpFlags where
  Global     == Global     = True
  IgnoreCase == IgnoreCase = True
  Multiline  == Multiline  = True
  _          == _          = False

JSRegExp : JSType
JSRegExp = JSObject "RegExp"

||| Creates a new RegExp with a list of flags.
newRegExp : String -> List RegExpFlags -> IO (JSValue JSRegExp)
newRegExp patt flags = do
  regex <- mkForeign (
      FFun "new RegExp(%0, %1)" [FString, FString] FPtr
    ) patt (mkFlags . nub $ flags)
  return $ MkJSObject regex
where
  mkFlags : List RegExpFlags -> String
  mkFlags (Global     :: fs) = "g" ++ mkFlags fs
  mkFlags (IgnoreCase :: fs) = "i" ++ mkFlags fs
  mkFlags (Multiline  :: fs) = "m" ++ mkFlags fs
  mkFlags []                 = ""

||| Uses a RegExp `regex` on the string `str`.
match : (str : String)
     -> (regex : JSValue JSRegExp)
     -> IO (JSValue JSArray)
match str regex = do
  res <- mkForeign (
      FFun "%0.match(%1)" [FString, FPtr] FPtr
    ) str (unpack regex)
  return $ MkJSObject res

||| Replaces matches of `regex` with `rpl` in the string `str`. Modifies
||| the original value.
replace : (str : String)
       -> (regex : JSValue JSRegExp)
       -> (rpl : String)
       -> IO String
replace str regex rpl =
  mkForeign (
      FFun "%0.replace(%1, %2)" [FString, FPtr, FString] FString
    ) str (unpack regex) rpl

||| Splits an array `str` at the occurences of `regex`.
split : (str : String)
     -> (regex : JSValue JSRegExp)
     -> IO (JSValue JSArray)
split str regex = do
  res <- mkForeign (
      FFun "%0.split(%1)" [FString, FPtr] FPtr
    ) str (unpack regex)
  return $ MkJSObject res
