module Log
  ( info,
    err,
  )
where

import System.IO (hPutStrLn, stderr)

info :: String -> IO ()
info = putStrLn

err :: String -> IO ()
err = hPutStrLn stderr
