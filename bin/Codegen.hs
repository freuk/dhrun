{-|
Module      : Main.hs
Description : dhrun main binary file.
Copyright   : (c) Valentin Reis, 2018
License     : MIT
Maintainer  : fre@freux.fr
-}
module Main
  ( main
  )
where

import Dhrun.Bin
import Protolude

main :: IO ()
main = codegen
