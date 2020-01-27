{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Main
  ( main
  )
where

{-import           Test.Tasty.Hspec-}

{-import qualified Data.Text                     as T-}
{-import           Data.Text.Arbitrary-}
{-import           Control.Monad.Mock-}
{-import           Control.Monad.Mock.TH-}
import Data.Text.Arbitrary
import Dhrun.Pure
import Dhrun.Types.Cfg
import Generic.Random
import Protolude hiding
  ( (<.>)
  )
import System.FilePath
import Test.Tasty hiding
  ( Timeout
  )
import Test.Tasty.Golden
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

instance Arbitrary (FileCheck Check) where

  arbitrary = genericArbitraryU

instance Arbitrary FileName where

  arbitrary = genericArbitraryU

instance Arbitrary WorkDir where

  arbitrary = genericArbitraryU

instance Arbitrary Check where

  arbitrary = genericArbitraryU

instance Arbitrary Pattern where

  arbitrary = genericArbitraryU

instance Arbitrary CommandName where

  arbitrary = genericArbitraryU

instance Arbitrary EnvVar where

  arbitrary = genericArbitraryU

instance Arbitrary Std where

  arbitrary = genericArbitraryU

instance Arbitrary VarValue where

  arbitrary = genericArbitraryU

instance Arbitrary VarName where

  arbitrary = genericArbitraryU

instance Arbitrary Arg where

  arbitrary = genericArbitraryU

instance Arbitrary Cmd where

  arbitrary = genericArbitraryU

instance Arbitrary CmdResult where

  arbitrary = genericArbitraryU

goldenYml :: FilePath -> TestTree
goldenYml fn =
  testCase
    (toS fn)
    ( do
      ymlCfg <- decodeCfgFile (toS $ fn <.> ".yml")
      dhCfg <- inputCfg (toS $ "./" <> fn <.> "dh")
      assertEqual "files not equal" ymlCfg dhCfg
    )

main :: IO ()
main = do
  goldenTests <-
    testGroup "Golden tests" <$>
      ( findByExtension [".yml"] "examples" <&>
        fmap (goldenYml . toS . dropExtension)
      )
  defaultMain $ testGroup "Tests" [unitTests, goldenTests, qcProps]

unitTests :: TestTree
unitTests =
  testGroup
    "HUnit tests"
    [ testCase "Pure.envVars" $
        envVars [EnvVar {varname = VarName "JOBVAR", value = VarValue "JOBVAL"}]
          [VarName "PASSME"]
          [("PASSME", "5"), ("DISCARDME", "9")] @?=
        [("JOBVAR", "JOBVAL"), ("PASSME", "5")]
    ]

qcProps :: TestTree
qcProps =
  testGroup
    "QuickCheck specs"
    [ QC.testProperty "concludeCmd fail/success patterns" $ \cmdresult -> shouldConclude cmdresult (concludeCmd True cmdresult)
    ]
  where
    shouldConclude :: CmdResult -> Either a b -> Bool
    shouldConclude cmdresult concluded = case cmdresult of
      DiedLegal {} -> True
      FoundAll {} -> isRight concluded
      DiedExpected {} -> isRight concluded
      _ -> isLeft concluded
