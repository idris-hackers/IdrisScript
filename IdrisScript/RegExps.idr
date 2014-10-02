module IdrisScript.RegExps

import IdrisScript

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

new : String -> List RegExpFlags -> IO (JSValue JSRegExp)
new patt flags = do
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

match : String -> JSValue JSRegExp -> IO (JSValue JSArray)
match str regex = do
  res <- mkForeign (
      FFun "%0.match(%1)" [FString, FPtr] FPtr
    ) str (unpack regex)
  return $ MkJSObject res

replace : String -> JSValue JSRegExp -> String -> IO String
replace str regex rpl =
  mkForeign (
      FFun "%0.replace(%1, %2)" [FString, FPtr, FString] FString
    ) str (unpack regex) rpl

split : String -> JSValue JSRegExp -> IO (JSValue JSArray)
split str regex = do
  res <- mkForeign (
      FFun "%0.split(%1)" [FString, FPtr] FPtr
    ) str (unpack regex)
  return $ MkJSObject res

