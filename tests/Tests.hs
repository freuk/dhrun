{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}
{-# language FlexibleContexts #-}
{-# language NoImplicitPrelude #-}

module Main
  ( main
  )
where

import           Protolude               hiding ( (<.>) )
import           Test.Tasty              hiding ( Timeout )
import           Test.Tasty.HUnit
import           Test.Tasty.Golden
import           System.FilePath
{-import           Test.Tasty.Hspec-}
import           Test.Tasty.QuickCheck         as QC
{-import qualified Data.Text                     as T-}
{-import           Data.Text.Arbitrary-}
{-import           Control.Monad.Mock-}
{-import           Control.Monad.Mock.TH-}

import           Dhrun.Pure
import           Dhrun.Types.Cfg
import           Generic.Random

{-instance Arbitrary CmdResult where-}
  {-arbitrary = genericArbitrary' Z uniform-}

goldenYml :: FilePath -> TestTree
goldenYml fn = goldenVsString
  fn
  (fn <.> ".yml")
  (toS . encodeCfg <$> inputCfg (toS $ "./" <> fn <.> "dh"))

main :: IO ()
main = do
  goldenTests <-
    testGroup "Golden tests"
      <$> (   findByExtension [".yml"] "examples"
          <&> fmap (goldenYml . toS . dropExtension)
          )
  defaultMain $ testGroup "Tests" [unitTests, goldenTests]

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
  {-[ QC.testProperty "concludeCmd fail/success patterns"-}
      {-$ \cmdresult -> shouldConclude cmdresult (concludeCmd True cmdresult)-}
  {-]-}
 {-where-}
  {-shouldConclude :: CmdResult -> Either a b -> Bool-}
  {-shouldConclude cmdresult concluded = case cmdresult of-}
    {-Timeout _ -> isLeft concluded-}
