{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language LambdaCase #-}
{-# language FlexibleContexts #-}
{-# language NoImplicitPrelude #-}

module Main
  ( main
  )
where

import           Protolude
{-import           Test.Tasty-}
{-import           Test.Tasty.HUnit-}
{-import           Test.Tasty.Hspec-}
{-import           Test.Tasty.QuickCheck         as QC-}
{-import qualified Data.Text                     as T-}
{-import           Data.Text.Arbitrary-}
{-import           Control.Monad.Mock-}
{-import           Control.Monad.Mock.TH-}

import           Dhrun.Types
import           Dhrun.AesonTypes
import           Data.ByteString.Char8
import           Control.Monad.Writer
import           System.IO.Error

data Result = Success | Failure
testDY :: (MonadIO m, MonadWriter [Text] m) => Text -> m Result
testDY fn = do

  tell ["loading " <> fn <> ".yml:"]
  loadedY <- decodeDhallExec $ "./examples/" <> fn <> "/" <> fn <> ".yml"
  for_ (lines $ encodeDhallExec loadedY) $ \x -> tell [toS x]

  tell ["loading " <> fn <> ".dh:"]
  loadedD <- inputDhallExec $ "./examples/" <> fn <> "/" <> fn <> ".dh"
  for_ (lines $ encodeDhallExec loadedD) $ \x -> tell [toS x]

  if loadedY == loadedD
    then tell ["success."] >> return Success
    else return Failure

main :: IO ()
main = for_ ["simple", "full", "two"] $ \testName ->
  runWriterT (runExceptT (testDY testName)) >>= \case
    (Right Failure, es) ->
      putText
          (  "Test \""
          <> testName
          <> "\" failure: .yml/.dh produced different configs.\nError log:"
          )
        >> for_ es putText
    (Right Success, _) ->
      putText
        $  "Test \""
        <> testName
        <> "\" success: .yml/.dh produced identical configs."
    (Left (e :: IOError), es) ->
      putText ("Failure in test " <> testName <> " :")
        >> print e
        >> putText "with test log:"
        >> for_ es putText
