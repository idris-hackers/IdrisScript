module IdrisScript.Timer

import IdrisScript

%access public export

export
record Timeout where
  constructor MkTimeout
  unTimeout : Ptr

export
record Interval where
  constructor MkInterval
  unInterval : Ptr

||| Executes a JS_IO action after `millis` milliseconds.
setTimeout : (() -> JS_IO ()) -> (millis : Int) -> JS_IO Timeout
setTimeout f millis = do
  timeout <- jscall "setTimeout(%0, %1)"
                    (JsFn (() -> JS_IO ()) -> Int -> JS_IO Ptr)
                    (MkJsFn f) millis
  pure $ MkTimeout timeout

||| Clears a timeout.
export
clearTimeout : Timeout -> JS_IO ()
clearTimeout timeout =
  jscall "clearTimeout(%0)" (Ptr -> JS_IO ()) (unTimeout timeout)

||| Periodically executes a JS_IO action after `millis` milliseconds.
setInterval : (() -> JS_IO ()) -> Int -> JS_IO Interval
setInterval f millis = do
  interval <- jscall "setInterval(%0, %1)"
                     (JsFn (() -> JS_IO ()) -> Int -> JS_IO Ptr)
                     (MkJsFn f) millis
  pure $ MkInterval interval

||| Clears an interval.
export
clearInterval : Interval -> JS_IO ()
clearInterval interval =
  jscall "clearInterval(%0)" (Ptr -> JS_IO ()) (unInterval interval)
