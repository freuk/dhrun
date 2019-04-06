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
import           Dhall
import           Data.Yaml
import           Data.ByteString.Char8
import           Control.Monad.Writer
import           System.IO.Error

data Result = Success | Failure
testDY
  :: (MonadIO m, MonadWriter [Text] m, MonadError IOError m) => Text -> m Result
testDY fn = do

  tell ["loading " <> fn <> ".yml:"]
  loadedY <- liftIO (try $ decodeFileEither (toS $ fn <> ".yml")) >>= \case
    Left  e          -> throwError e
    Right (Left  pa) -> throwError $ userError $ "parse fail:" <> show pa
    Right (Right a ) -> return a
  for_ (lines (encode loadedY)) $ \x -> tell [toS x]

  tell ["loading " <> fn <> ".dh:"]
  loadedD <- liftIO (try (input ft $ fn <> ".dh")) >>= \case
    Left  e -> throwError e
    Right d -> return d
  for_ (lines (encode loadedD)) $ \x -> tell [toS x]

  if loadedY == loadedD
    then tell ["success."] >> return Success
    else return Failure
 where
  ft :: Dhall.Type DhallExec
  ft = Dhall.auto

main :: IO ()
main = for_ ["simple", "full"] $ \name ->
  runWriterT (runExceptT (testDY $ "./examples/" <> name)) >>= \case
    (Right Failure, es) ->
      putText
          ("Test \""
          <> name
          <> "\" failure: .yml/.dh produced different configs.\nError log:"
          )
        >> for_ es putText
    (Right Success, _) ->
      putText
        $  "Test \""
        <> name
        <> "\" success: .yml/.dh produced identical configs."
    (Left (e :: IOError), es) ->
      putText ("Failure in test " <> name <> " :")
        >> print e
        >> putText "with test log:"
        >> for_ es putText
