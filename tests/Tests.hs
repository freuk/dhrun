{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language FlexibleContexts #-}
{-# language NoImplicitPrelude #-}

module Main
  ( main
  )
where

import           Protolude
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Golden
{-import           Test.Tasty.Hspec-}
{-import           Test.Tasty.QuickCheck         as QC-}
{-import qualified Data.Text                     as T-}
{-import           Data.Text.Arbitrary-}
{-import           Control.Monad.Mock-}
{-import           Control.Monad.Mock.TH-}

import           Dhrun.Pure
import           Dhrun.Types.Cfg

testDY :: Text -> IO ()
testDY fn = do
  loadedD <- inputCfg $ "./examples/" <> fn <> "/" <> fn <> ".dh"
  writeBinaryFile (toS $ "./examples/" <> fn <> "/" <> fn <> ".yml")
                  (toS $ encodeCfg loadedD)

goldenYml :: Text -> TestTree
goldenYml folder = goldenVsFile (toS extless) golden output io
 where
  io      = (void . testDY) folder
  extless = "examples/" <> folder <> "/" <> folder
  golden  = toS $ extless <> ".yml"
  output  = toS $ extless <> ".yml_"

main :: IO ()
main = do
  let tests = testGroup
        "Tests"
        [ unitTests
        , testGroup "Golden tests" (goldenYml <$> ["simple", "two", "full"])
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
