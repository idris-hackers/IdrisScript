module Main

import IdrisScript
import IdrisScript.Objects
import IdrisScript.JSON

main : JS_IO ()
main = do
  let text = "{\"foo\":true,\"bar\":1337}"
  case !(parse text) of
       Just (_ ** obj) =>
         case !(getProperty "bar" obj) of
              Just (_ ** res) => log res
              _               => return ()
       _               => return ()
