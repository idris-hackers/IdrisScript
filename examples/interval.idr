module Main

import IdrisScript
import IdrisScript.Timer

main : IO ()
main = do
  setInterval (\_ => print "PING!") 1000
  putStrLn "Done!"
