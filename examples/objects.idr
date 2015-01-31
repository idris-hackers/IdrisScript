module Main

import IdrisScript
import IdrisScript.Objects

main : JS_IO ()
main = do
  obj <- empty

  setProperty "foo" (toJS {from=Int}{to=JSNumber} 666)  obj
  setProperty "bar" (toJS {from=Int}{to=JSNumber} 1337) obj

  keys <- keys obj

  log keys
  log obj
