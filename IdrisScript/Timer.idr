module IdrisScript.Timer

import IdrisScript

%access public

abstract
record Timeout : Type where
  MkTimeout : (unTimeout : Ptr) -> Timeout

abstract
record Interval : Type where
  MkInterval : (unInterval : Ptr) -> Interval

||| Executes an IO action after `millis` milliseconds.
setTimeout : (() -> IO ()) -> (millis : Int) -> IO Timeout
setTimeout f millis = do
  timeout <- mkForeign (
      FFun "setTimeout(%0, %1)" [FFunction FUnit (FAny (IO ())), FInt] FPtr
    ) f millis
  return $ MkTimeout timeout

||| Clears a timeout.
clearTimeout : Timeout -> IO ()
clearTimeout timeout =
  mkForeign (FFun "clearTimeout(%0)" [FPtr] FUnit) (unTimeout timeout)

||| Periodically executes an IO action after `millis` milliseconds.
setInterval : (() -> IO ()) -> Int -> IO Interval
setInterval f millis = do
  interval <- mkForeign (
      FFun "setInterval(%0, %1)" [FFunction FUnit (FAny (IO ())), FInt] FPtr
    ) f millis
  return $ MkInterval interval

||| Clears an interval.
clearInterval : Interval -> IO ()
clearInterval interval =
  mkForeign (FFun "clearInterval(%0)" [FPtr] FUnit) (unInterval interval)
