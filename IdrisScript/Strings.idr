module IdrisScript.Strings

import IdrisScript

%access public

||| Upper case a string.
toUpperCase : String -> IO String
toUpperCase str = mkForeign (FFun "%0.toUpperCase()" [FString] FString) str

||| Lower case a string.
toLowerCase : String -> IO String
toLowerCase str = mkForeign (FFun "%0.toLowerCase()" [FString] FString) str

||| Trims a string.
trim : String -> IO String
trim str = mkForeign (FFun "%0.trim()" [FString] FString) str

||| Trims a string from the left.
trimLeft : String -> IO String
trimLeft str = mkForeign (FFun "%0.trimLeft()" [FString] FString) str

||| Trims a string from the right.
trimRight : String -> IO String
trimRight str = mkForeign (FFun "%0.trimRight()" [FString] FString) str

||| Returns the position of the string `search` in the string `str`.
indexOf : (str : String) -> (search : String) -> IO Int
indexOf str search =
  mkForeign (FFun "%0.indexOf(%1)" [FString, FString] FInt) str search

||| Returns a substring from postion `start` with the length `length` from
||| the string `str`.
substr : (str : String)
      -> (start : Int)
      -> (length : Int)
      -> IO String
substr str start length =
  mkForeign (
      FFun "%0.substr(%1,%2)" [FString, FInt, FInt] FString
    ) str start length

||| Returns a substring from position `from` to postion `to` in the
||| string `str`.
substring : (str : String)
         -> (from : Int)
         -> (to : Int)
         -> IO String
substring str from to =
  mkForeign (
      FFun "%0.substring(%1,%2)" [FString, FInt, FInt] FString
    ) str from to
