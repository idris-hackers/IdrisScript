module IdrisScript.Strings

import IdrisScript

%access public export

||| Upper case a string.
toUpperCase : String -> JS_IO String
toUpperCase str = jscall "%0.toUpperCase()" (String -> JS_IO String) str

||| Lower case a string.
toLowerCase : String -> JS_IO String
toLowerCase str = jscall "%0.toLowerCase()" (String -> JS_IO String) str

||| Trims a string.
trim : String -> JS_IO String
trim str = jscall "%0.trim()" (String -> JS_IO String) str

||| Trims a string from the left.
trimLeft : String -> JS_IO String
trimLeft str = jscall "%0.trimLeft()" (String -> JS_IO String) str

||| Trims a string from the right.
trimRight : String -> JS_IO String
trimRight str = jscall "%0.trimRight()" (String -> JS_IO String) str

||| Returns the position of the string `search` in the string `str`.
indexOf : (str : String) -> (search : String) -> JS_IO Int
indexOf str search =
  jscall "%0.indexOf(%1)" (String -> String -> JS_IO Int) str search

||| Returns a substring from postion `start` with the length `length` from
||| the string `str`.
substr : (str : String)
      -> (start : Int)
      -> (length : Int)
      -> JS_IO String
substr str start length =
  jscall "%0.substr(%1,%2)" (String -> Int -> Int -> JS_IO String)
          str start length

||| Returns a substring from position `from` to postion `to` in the
||| string `str`.
substring : (str : String)
         -> (from : Int)
         -> (to : Int)
         -> JS_IO String
substring str from to =
  jscall "%0.substring(%1,%2)" (String -> Int -> Int -> JS_IO String)
          str from to
