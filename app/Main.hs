module Main where

import Lib
import Control.Monad.Exception (catch)

main :: IO ()
main = catch (serveBot) (main)
