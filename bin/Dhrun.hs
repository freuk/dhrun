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

import Protolude
import Dhrun.Bin

main :: IO ()
main = cli
