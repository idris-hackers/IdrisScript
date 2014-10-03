module Main

import IdrisScript
import IdrisScript.Arrays

main : IO ()
main = do
  arr <- toJSArray {from=Int} {to=JSNumber} [1..100]
  forEach (\elm => log (getProof !(pack elm))) arr
