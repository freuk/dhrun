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

import           Dhrun.Types.Cfg
import qualified Data.ByteString.Char8         as B8
import           Control.Monad.Writer
import           System.IO.Error

data Result = Success | Failure deriving (Eq)
testDY :: (MonadIO m, MonadWriter [Text] m) => Text -> m Result
testDY fn = do

  tell ["loading " <> fn <> ".yml:"]
  loadedY <- decodeCfgFile $ "./examples/" <> fn <> "/" <> fn <> ".yml"
  for_ (B8.lines $ encodeCfg loadedY) $ \x -> tell [toS x]

  tell ["loading " <> fn <> ".dh:"]
  loadedD <- inputCfg $ "./examples/" <> fn <> "/" <> fn <> ".dh"
  for_ (B8.lines $ encodeCfg loadedD) $ \x -> tell [toS x]

  if loadedY == loadedD
    then tell ["success."] >> return Success
    else return Failure

main :: IO ()
main = do
  fileLoadingResults <- for ["simple", "full", "two"] $ \testName ->
    (runWriterT . runExceptT . testDY) testName >>= \case
      (Right Failure, es) ->
        putText
            (  "Test \""
            <> testName
            <> "\" failure: .yml/.dh produced different configs.\nError log:"
            )
          >> for_ es putText
          >> return Success
      (Right Success, _) ->
        putText
            (  "Test \""
            <> testName
            <> "\" success: .yml/.dh produced identical configs."
            )
          >> return Success
      (Left (e :: IOError), es) ->
        putText ("Failure in test " <> testName <> " :")
          >> print e
          >> putText "with test log:"
          >> for_ es putText
          >> return Failure
  when (Failure `elem` fileLoadingResults) $ die "test set failure."
