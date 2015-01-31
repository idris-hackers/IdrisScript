module Main

import IdrisScript
import IdrisScript.RegExps
import IdrisScript.Arrays

main : JS_IO ()
main = do
  args  <- with Arrays empty
  args `push` (toJS {to=JSString} "Pinkie Pie")
  args `push` (toJS {to=JSString} "g")

  regex <- new !RegExp args
  
  let text = "Pinkie Pie is best pony!"

  case regex of
       ("RegExp" ** r) => putStrLn !(replace text r "Rainbow Dash")
       _               => putStrLn "Whoops"

