{-# language OverloadedStrings #-}
{-# language StandaloneDeriving #-}
{-# language GADTs #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language MultiParamTypeClasses #-}
{-# language QuasiQuotes #-}
{-# language TemplateHaskell #-}
{-# language NoImplicitPrelude #-}

module Main (main) where

import           Protolude
{-import           Test.Tasty-}
{-import           Test.Tasty.HUnit-}
{-import           Test.Tasty.Hspec-}
{-import           Test.Tasty.QuickCheck         as QC-}
{-import qualified Data.Text                     as T-}
{-import           Data.Text.Arbitrary-}
{-import           Control.Monad.Mock-}
{-import           Control.Monad.Mock.TH-}

import           Dhallexec.Types
import Dhall

main :: IO ()
main = do
  loaded <- detailed $ input ft "./examples/singleProcess.dhall"
  print loaded
 where
  ft :: Dhall.Type DhallExec
  ft = Dhall.auto
