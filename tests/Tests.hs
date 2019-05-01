{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language FlexibleContexts #-}
{-# language NoImplicitPrelude #-}

module Main
  ( main
  )
where

import           Protolude hiding ((<.>))
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Golden
import           System.FilePath
{-import           Test.Tasty.Hspec-}
{-import           Test.Tasty.QuickCheck         as QC-}
{-import qualified Data.Text                     as T-}
{-import           Data.Text.Arbitrary-}
{-import           Control.Monad.Mock-}
{-import           Control.Monad.Mock.TH-}

import           Dhrun.Pure
import           Dhrun.Types.Cfg

testDY :: (MonadIO m, StringConv ByteString b) => FilePath -> m b
testDY fn = do
  loadedD <- inputCfg $ toS $"./"<> fn <.> "dh"
  return $ toS $ encodeCfg loadedD

goldenYml :: FilePath -> TestTree
goldenYml fn = goldenVsString fn golden io
 where
  io     = testDY fn
  golden = fn <.> ".yml"

main :: IO ()
main = do
  paths <- findByExtension [".yml"] "examples"
  let tests = testGroup
        "Tests"
        [ unitTests
        , testGroup "Golden tests" (goldenYml . toS . dropExtension <$> paths)
        ]
  defaultMain tests

unitTests :: TestTree
unitTests = testGroup
  "HUnit tests"
  [ testCase "Pure.envVars"
    $   envVars [EnvVar {varname = VarName "JOBVAR", value = VarValue "JOBVAL"}]
                [VarName "PASSME"]
                [("PASSME", "5"), ("DISCARDME", "9")]
    @?= [("JOBVAR", "JOBVAL"), ("PASSME", "5")]
  ]

{-qcProps :: TestTree-}
{-qcProps = testGroup-}
  {-"QuickCheck specs"-}
  {-[ QC.testProperty "checkAuthors, AllAuthorsCredited" $ \authorList ->-}
      {-checkAuthors-}
          {-(AuthorFileContents (T.intercalate "\n" (authorList :: [Text])))-}
          {-(GitAuthorsList authorList)-}
        {-== AllAuthorsCredited-}
  {-]-}
