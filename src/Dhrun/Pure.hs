{-# language DerivingStrategies #-}
{-# language FlexibleContexts #-}
{-# language RecordWildCards #-}
{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language ScopedTypeVariables #-}
{-# language TypeOperators #-}
{-# language NoImplicitPrelude #-}

{-|
Module      : Dhrun.Run
Description : runner
Copyright   : (c) Valentin Reis, 2018
License     : MIT
Maintainer  : fre@freux.fr
-}

module Dhrun.Pure
  ( ProcessWas(..)
  , CmdResult(..)
  , MonitoringResult(..)
  , Std(..)
  , envVars
  , mapTuple
  , finalize
  )
where

import           Dhrun.Types.Cfg
import           Protolude
import qualified Data.Map.Lazy                 as DM
                                                ( fromList
                                                , toList
                                                )
import qualified Data.Map.Merge.Lazy           as DMM
                                                ( merge
                                                , preserveMissing
                                                , zipWithMatched
                                                )
import           Control.Arrow                  ( (***) )


data CmdResult =
    Timeout Cmd
  | DiedLegal Cmd
  | ThrewException Cmd IOException
  | DiedFailure Cmd Int
  | FoundAll Cmd
  | FoundIllegal Cmd Text Std
  | OutputLacking Cmd Std
  | ConduitException Cmd Std
  deriving (Show)

data MonitoringResult =
    ThrowFoundAllWants
  | ThrowFoundAnAvoid Text
  deriving (Show, Typeable)
instance Exception MonitoringResult

data ProcessWas = Died ExitCode | Killed

data Std = Out | Err deriving (Show)

noChecks :: Cmd -> Bool
noChecks Cmd {..} =
  null (wants $ filecheck out) && null (wants $ filecheck err)

envVars :: [EnvVar] -> [VarName] -> [(Text, Text)] -> [(Text, Text)]
envVars internEnv passVars externEnv = DM.toList $ DMM.merge
  DMM.preserveMissing
  DMM.preserveMissing
  (DMM.zipWithMatched (\_ x _ -> x))
  (DM.fromList (forcedEnvVars internEnv))
  (DM.fromList (externEnvVars passVars externEnv))

externEnvVars :: [VarName] -> [(Text, Text)] -> [(Text, Text)]
externEnvVars passvars = filter (\(k, _) -> k `elem` (toS <$> passvars))

forcedEnvVars :: [EnvVar] -> [(Text, Text)]
forcedEnvVars vs = (\EnvVar {..} -> (toS varname, toS value)) <$> vs

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple = join (***)


finalize
  :: Cmd
  -> Either (Either SomeException ()) (Either SomeException ())
  -> ProcessWas
  -> CmdResult
finalize c _ (Died (ExitFailure n)) = DiedFailure c n
finalize c (Left (Right ())) _ =
  if noChecks c then DiedLegal c else OutputLacking c Out
finalize c (Right (Right ())) _ =
  if noChecks c then DiedLegal c else OutputLacking c Err
finalize c (Left (Left e)) _ = case fromException e of
  Just (ThrowFoundAnAvoid t) -> FoundIllegal c t Out
  Just ThrowFoundAllWants    -> FoundAll c
  Nothing                    -> ConduitException c Out
finalize c (Right (Left e)) _ = case fromException e of
  Just (ThrowFoundAnAvoid t) -> FoundIllegal c t Err
  Just ThrowFoundAllWants    -> FoundAll c
  Nothing                    -> ConduitException c Err
