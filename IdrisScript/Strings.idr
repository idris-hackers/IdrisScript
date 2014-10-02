module IdrisScript.Strings

import IdrisScript

%access public

toUpperCase : String -> IO String
toUpperCase str = mkForeign (FFun "%0.toUpperCase()" [FString] FString) str

toLowerCase : String -> IO String
toLowerCase str = mkForeign (FFun "%0.toLowerCase()" [FString] FString) str

trim : String -> IO String
trim str = mkForeign (FFun "%0.trim()" [FString] FString) str

trimLeft : String -> IO String
trimLeft str = mkForeign (FFun "%0.trimLeft()" [FString] FString) str

trimRight : String -> IO String
trimRight str = mkForeign (FFun "%0.trimRight()" [FString] FString) str

indexOf : String -> IO Int
indexOf str = mkForeign (FFun "%0.indexOf()" [FString] FInt) str

substr : String -> Int -> Int -> IO String
substr str start length =
  mkForeign (
      FFun "%0.substr(%1,%2)" [FString, FInt, FInt] FString
    ) str start length

substring : String -> Int -> Int -> IO String
substring str from to =
  mkForeign (
      FFun "%0.substring(%1,%2)" [FString, FInt, FInt] FString
    ) str from to
