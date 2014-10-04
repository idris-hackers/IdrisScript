module Main

import IdrisScript
import IdrisScript.Timer

main : IO ()
main = do
  interval <- setInterval (\_ => print "PING!") 1000
  putStrLn "Done!"
