{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Bin.hs
Description : dhall-exec main app file.
Copyright   : (c) Valentin Reis, 2018
License     : MIT
Maintainer  : fre@freux.fr
-}
module Dhrun.Bin
  ( cli
  , codegen
  )
where

import qualified Data.Aeson as J
import Data.Aeson.Encode.Pretty as AP (encodePretty)
import Data.Aeson.Extra.Merge
import qualified Data.ByteString as B (getContents)
import Data.Default
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty
import qualified Data.Yaml as Y
import qualified Dhall
import qualified Dhall.Core as Dhall
import Dhall.Import as Dhall
import Dhall.JSON as DJ
import Dhall.JSONToDhall as JSONToDhall
import qualified Dhall.Lint as Lint
import qualified Dhall.Src as Dhall
import qualified Dhall.TypeCheck as Dhall
import Dhrun.Run
import Dhrun.Types.Cfg
import GHC.IO.Encoding
import NeatInterpolation
import Options.Applicative
import Protolude
import System.Directory
import System.FilePath.Posix
import qualified System.IO
import qualified System.IO as SIO

cli :: IO ()
cli =
  parseDhrunCli >>= runDhrun

codegen :: IO ()
codegen = do
  Dhall.load (Lint.lint $ Dhall.absurd <$> Dhall.embed (Dhall.injectWith Dhall.defaultInterpretOptions) (def :: Cfg)) >>=
    exprToDir "defaults/" "Cfg"
  where
    exprToDir dir defName expr = do
      let (dest, destJ, destY) = mkPaths dir defName
      DJ.dhallToJSON expr & \case
        Left e -> die $ "horrible internal dhall error: " <> show e
        Right jsonValue -> do
          putText $ "  Writing default for " <> defName <> " to " <> dest <> "."
          createDirectoryIfMissing True (takeDirectory $ toS dest)
          writeOutput
            licenseDhall
            (toS dest)
            expr
          writeFile (toS destJ) $ toS (AP.encodePretty jsonValue)
          writeFile (toS destY) $ licenseYaml <> toS (Y.encode jsonValue)
    resourcePath dir defName x = "./resources" <> dir <> defName <> x
    mkPaths :: Text -> Text -> (Text, Text, Text)
    mkPaths dir defName =
      ( resourcePath dir defName ".dhall"
      , resourcePath dir defName ".json"
      , resourcePath dir defName ".yaml"
      )

data MainCfg
  = MainCfg
      { useStdin :: Bool
      , argInput :: Maybe Text
      , configType :: SourceType
      }

commonParser :: Parser MainCfg
commonParser =
  MainCfg <$>
    flag
      False
      True
      (long "stdin" <> short 'i' <> help "Read configuration on stdin.") <*>
    optional
      ( strArgument
        ( metavar "CONFIG" <>
          help
            "Input configuration with .yml/.yaml/.dh/.dhall extension. Leave void for stdin (dhall) input."
        )
      ) <*>
    flag
      Dhall
      Yaml
      ( long "yaml" <> short 'y' <>
        help
          "Assume configuration to be yaml(json is valid yaml) instead of dhall."
      )

opts :: Parser (IO Cfg)
opts = (Dhrun.Bin.load <$> commonParser) <**> helper

data SourceType = Dhall | Yaml | Json
  deriving (Eq)

data FinallySource = UseDefault | NoExt | FinallyFile SourceType Text | FinallyStdin SourceType

ext :: Bool -> SourceType -> Maybe Text -> FinallySource
ext _ _ (Just fn)
  | xt `elem` [".dh", ".dhall"] = FinallyFile Dhall fn
  | xt `elem` [".yml", ".yaml"] = FinallyFile Yaml fn
  | xt `elem` [".json"] = FinallyFile Json fn
  | otherwise = NoExt
  where
    xt = takeExtension $ toS fn
ext useStdin st Nothing = if useStdin then FinallyStdin st else UseDefault

load :: MainCfg -> IO Cfg
load MainCfg {..} =
  case ext useStdin configType argInput of
    UseDefault -> return def
    (FinallyFile sourceType filename) ->
      makeAbsolute (toS filename) >>= readFile >>= (process sourceType . toS)
    (FinallyStdin sourceType) ->
      B.getContents >>= process sourceType
    NoExt ->
      argInput & \case
        Nothing -> return def
        Just s -> process configType (toS s)
  where
    process = processType (Proxy :: Proxy Cfg)

processType
  :: (Default x, Dhall.Interpret x, Dhall.Inject x)
  => (Proxy x)
  -> SourceType
  -> ByteString
  -> IO x
processType proxy@(Proxy :: Proxy x) sourceType bs =
  mergeAndExtract (def :: x) =<< toExpr proxy sourceType bs

toExpr
  :: (Dhall.Inject x, Dhall.Interpret x, Default x)
  => (Proxy x)
  -> SourceType
  -> ByteString
  -> IO (Dhall.Expr Dhall.Src Dhall.X)
toExpr _proxy (Dhall) s = Dhall.inputExpr $ toS s
toExpr proxy (Yaml) s = sourceValueToExpr proxy $ Y.decodeEither' s
toExpr proxy (Json) s = sourceValueToExpr proxy $ J.eitherDecode' (toS s)

sourceValueToExpr
  :: (Default x, Dhall.Interpret x, Dhall.Inject x)
  => (Proxy x)
  -> Either e Y.Value
  -> IO (Dhall.Expr Dhall.Src Dhall.X)
sourceValueToExpr (Proxy :: Proxy x) = \case
  Left _ -> die "yaml parsing exception"
  Right v -> do
    DJ.dhallToJSON exprValue & \case
      Left e -> die $ "horrible internal dhall error in cli parsing: " <> show e
      Right jsonValue ->
        JSONToDhall.dhallFromJSON
          JSONToDhall.defaultConversion
          exprType
          (lodashMerge jsonValue v) & \case
          Left e -> die ("yaml -> dhall compilation error" <> show e)
          Right expr -> return expr
  where
    exprType :: Dhall.Expr Dhall.Src Dhall.X
    exprType = typeToExpr (Proxy :: Proxy x)
    exprValue = valueToExpr (def :: x)

mergeAndExtract
  :: (Dhall.Interpret x, Dhall.Inject x)
  => x
  -> Dhall.Expr Dhall.Src Dhall.X
  -> IO x
mergeAndExtract x expr =
  Dhall.extract
    Dhall.auto
    ( Dhall.normalize
      ( Dhall.Prefer
        (valueToExpr x)
        expr
      )
    ) & \case
    Nothing -> die "dhall extraction error"
    Just a -> return a

typeToExpr :: Dhall.Interpret x => Proxy x -> Dhall.Expr Dhall.Src b
typeToExpr (Proxy :: Proxy x) =
  Dhall.absurd <$> Dhall.expected (Dhall.auto :: Dhall.Type x)

valueToExpr :: (Dhall.Inject x) => x -> Dhall.Expr Dhall.Src Dhall.X
valueToExpr x =
  Dhall.absurd <$>
    Dhall.embed
      (Dhall.injectWith Dhall.defaultInterpretOptions)
      x

customExecParserArgs :: [Text] -> ParserPrefs -> ParserInfo a -> IO a
customExecParserArgs args pprefs pinfo =
  handleParseResult $ execParserPure pprefs pinfo (toS <$> args)

parseDhrunCli :: IO Cfg
parseDhrunCli = fmap toS <$> getArgs >>= parseCli "dhrun" "Dhrun" opts

parseCli :: Text -> Text -> Parser (IO a) -> [Text] -> IO a
parseCli h d x args =
  GHC.IO.Encoding.setLocaleEncoding SIO.utf8 >>
    ( join .
      customExecParserArgs args (prefs showHelpOnError) $
      info
        (helper <*> x)
        ( fullDesc <> header (toS h) <>
          progDesc (toS d)
        )
    )

writeOutput :: (Pretty.Pretty a) => Text -> FilePath -> Dhall.Expr s a -> IO ()
writeOutput hh dest e =
  System.IO.withFile dest System.IO.WriteMode $ \hnd -> do
    System.IO.hPutStrLn hnd (toS hh)
    Pretty.renderIO
      hnd $
      Pretty.layoutSmart
        prettyOpts
        (Pretty.pretty e)
    System.IO.hPutStr hnd "\n"

prettyOpts :: Pretty.LayoutOptions
prettyOpts =
  Pretty.defaultLayoutOptions
    { Pretty.layoutPageWidth = Pretty.AvailablePerLine 80 1.0
    }

-- | A license for Yaml files
licenseYaml :: Text
licenseYaml =
  [text|
    # ******************************************************************************
    #  Copyright 2020 Valentin Reis.
    #  (c.f. AUTHORS, LICENSE)
    #
    #  SPDX-License-Identifier: MIT
    # ******************************************************************************
    #
    #     this file is generated, modifications will be erased.
    #

  |]

-- | A license for Dhall files
licenseDhall :: Text
licenseDhall =
  [text|
    -- ******************************************************************************
    --  Copyright 2020 Valentin Reis.
    --  (c.f. AUTHORS, LICENSE)
    --
    --  SPDX-License-Identifier: MIT
    -- ******************************************************************************
    --
    --     this file is generated, modifications will be erased.
    --

  |]
