module Client where

import FFI

mylog :: String -> Fay ()
mylog = ffi "console.log(%1)"

setTimeout :: Double ->  Fay () -> Fay Double
setTimeout  = ffi "setTimeout(%2, %1)"

setInterval :: Double ->  Fay () ->  Fay Double
setInterval = ffi "setInterval(%2, %1)"

main :: Fay ()
main = do setInterval 1000 (mylog "WORLD!")
          return ()
