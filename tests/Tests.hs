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

import Data.Text.Arbitrary
import Dhrun.Bin
import Dhrun.Pure
import Dhrun.Run
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

testExample :: ([Text] -> Bool) -> (Text, Cfg) -> TestTree
testExample assertion (name, cfg) =
  testCase (toS name)
    ( do
      out <- runDhrun (cfg {verbosity = Normal})
      assertBool (show out) (assertion out)
    )

goldenLoad :: FilePath -> TestTree
goldenLoad fn =
  testCase
    (toS fn)
    ( do
      yamlCfg <-
        processType (Proxy :: Proxy Cfg) Yaml =<<
          (toS <$> readFile (toS $ "./" <> fn <.> "yaml"))
      jsonCfg <-
        processType (Proxy :: Proxy Cfg) Json =<<
          (toS <$> readFile (toS $ "./" <> fn <.> "json"))
      dhCfg <-
        processType (Proxy :: Proxy Cfg) Dhall =<<
          (toS <$> readFile (toS $ "./" <> fn <.> "dhall"))
      assertEqual "yaml/dhall do not result in equal Cfg" yamlCfg dhCfg
      assertEqual "json/dhall do not result in equal cfg" jsonCfg dhCfg
    )

main :: IO ()
main = do
  goldenTestsSuccess <-
    testGroup "Golden tests for success files" <$>
      ( findByExtension [".yaml"] "resources/examples-successes" <&>
        fmap (goldenLoad . toS . dropExtension)
      )
  goldenTestsFailure <-
    testGroup "Golden tests for failure files" <$>
      ( findByExtension [".yaml"] "resources/examples-failures" <&>
        fmap (goldenLoad . toS . dropExtension)
      )
  defaultMain $
    testGroup "Tests"
      [ unitTests
      , goldenTestsSuccess
      , goldenTestsFailure
      , qcProps
      , testGroup "successes"
        (testExample (\out -> out == []) <$> (successes examples))
      , testGroup "failures"
        (testExample (\out -> out /= []) <$> (failures examples))
      ]

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
