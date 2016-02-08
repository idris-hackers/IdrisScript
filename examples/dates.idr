module Main

import IdrisScript
import IdrisScript.Date

main : JS_IO ()
main = do
  current <- now
  if !(getDay current) == Friday
     then putStrLn' "It's Friday! I'm in love!"
     else putStrLn' "Meh!"
