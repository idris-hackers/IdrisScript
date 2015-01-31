module Main

import IdrisScript
import IdrisScript.Timer

main : JS_IO ()
main = do
  setInterval (\_ => print "PING!") 1000
  putStrLn "Done!"
