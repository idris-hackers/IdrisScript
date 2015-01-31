module Main

import IdrisScript
import IdrisScript.Arrays

main : JS_IO ()
main = do
  arr <- toJSArray {from=Int} {to=JSNumber} [1..100]
--   log arr
  forEach (\elm => log (getProof !(pack elm))) arr
