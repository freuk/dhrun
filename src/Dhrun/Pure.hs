{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Dhrun.Run
Description : runner
Copyright   : (c) Valentin Reis, 2018
License     : MIT
Maintainer  : fre@freux.fr
-}
module Dhrun.Pure
  ( ProcessWas (..)
  , CmdResult (..)
  , MonitoringResult (..)
  , Std (..)
  , envVars
  , concludeCmd
  , finalizeCmd
  , getWdFilename
  , stdToS
  , noWants
  , with3
  , with2
  , mapTuple
  )
where

import Control.Arrow
  ( (***)
  )
import qualified Data.Map.Lazy as DM
  ( fromList
  , toList
  )
import qualified Data.Map.Merge.Lazy as DMM
  ( merge
  , preserveMissing
  , zipWithMatched
  )
import qualified Data.Text as T
  ( lines
  )
import Dhrun.Types.Cfg
import Protolude

data CmdResult
  = Timeout Cmd -- | if the command died with a timeout
  | DiedLegal Cmd -- | if the command wasnt expected to do anything and died 1
  | ThrewException Cmd Text
  | DiedFailure Cmd Int
  | DiedExpected Cmd -- | if the command was expected in exitcode x and died x
  | DiedUnExpected Cmd ExitCode -- | command expected in exitcode x and died y!=x
  | FoundAll Cmd
  | FoundIllegal Cmd Text Std
  | OutputLacking Cmd Std
  | ConduitException Cmd Std
  deriving (Show, Generic)

data MonitoringResult
  = ThrowFoundAllWants
  | ThrowFoundAnAvoid Text
  deriving (Show, Typeable)

instance Exception MonitoringResult

data ProcessWas = Died ExitCode | Killed

data Std = Out | Err
  deriving (Show, Generic)

-- | concludeCmd hadNoWants cmdresult returns dhrun's conclusion based on whether
-- there were any "wants" in the template.
concludeCmd :: Bool -> CmdResult -> Either [Text] Text
concludeCmd True (DiedLegal _) = Right "All commands exited successfully."
concludeCmd False (DiedLegal c) =
  Left
    [ "command exited:" <>
        mconcat (intersperse "\n" (T.lines (show c)))
    ]
concludeCmd _ (Timeout c) =
  Left $ "The following command timed out:" : T.lines (show c)
concludeCmd _ (DiedFailure c n) =
  Left $
    "The following command died with exit code " <>
    show n <>
    " :" :
    T.lines (show c)
concludeCmd _ (FoundAll c) =
  Right
    ( "All searched patterns in the following command were found. Killing all processes.\n " <>
      mconcat (intersperse "\n" (T.lines (show c)))
    )
concludeCmd _ (FoundIllegal c t e) =
  Left $
    "The illegal pattern " <>
    t <>
    " was found in the output of this process' " <>
    stdToS e <>
    ":" :
    T.lines (show c)
concludeCmd _ (OutputLacking c e) =
  Left $
    "This process' " <>
    stdToS e <>
    " was found to be lacking pattern(s):" :
    T.lines (show c)
concludeCmd _ (ConduitException c e) =
  Left $ "This process ended with a conduit exception:" <> stdToS e :
    T.lines
      (show c)
concludeCmd _ (ThrewException c e) =
  Left $ "This process' execution ended with an exception: " <> e :
    T.lines
      (show c)
concludeCmd _ (DiedUnExpected c n) =
  Left $ "process exited with inadequate exit code " <> show n <> ": " :
    T.lines
      (show c)
concludeCmd _ (DiedExpected c) =
  Right $ "process exited with adequate exit code " <> ": " <> show c

stdToS :: Std -> Text
stdToS Out = "stdout"
stdToS Err = "stderr"

noWants :: Cmd -> Bool
noWants Cmd {..} = null (wants $ filecheck out) && null (wants $ filecheck err)

envVars :: [EnvVar] -> [VarName] -> [(Text, Text)] -> [(Text, Text)]
envVars internEnv passVars externEnv =
  DM.toList $
    DMM.merge
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

getWdFilename :: Text -> FileCheck a -> FilePath
getWdFilename wdT fc = toS $ wdT <> "/" <> toS (filename fc)

finalizeCmd
  :: Cmd
  -> Either (Either SomeException ()) (Either SomeException ())
  -> ProcessWas
  -> CmdResult
finalizeCmd c _ (Died ec) = case exitcode c of
  Nothing -> DiedLegal c
  Just ecExpect ->
    if ecExpect == ec then DiedExpected c else DiedUnExpected c ec
finalizeCmd c ei _ = rightFinalizer ei
  where
    rightFinalizer (Left (Right ())) = legalOrLacking Out
    rightFinalizer (Right (Right ())) = legalOrLacking Err
    rightFinalizer (Left (Left e)) = unpackE e Out
    rightFinalizer (Right (Left e)) = unpackE e Err
    unpackE e r = case fromException e of
      Just (ThrowFoundAnAvoid t) -> FoundIllegal c t r
      Just ThrowFoundAllWants -> FoundAll c
      Nothing -> ConduitException c r
    legalOrLacking r = if noWants c then DiedLegal c else OutputLacking c r

with3
  :: ((t1 -> t2) -> t3)
  -> ((t4 -> t5) -> t2)
  -> ((t6 -> t7) -> t5)
  -> (t1 -> t4 -> t6 -> t7)
  -> t3
with3 w1 w2 w3 f = w1 $ \v1 -> w2 $ \v2 -> w3 $ \v3 -> f v1 v2 v3

with2 :: ((t1 -> t2) -> t3) -> ((t4 -> t5) -> t2) -> (t1 -> t4 -> t5) -> t3
with2 w1 w2 f = w1 $ \v1 -> w2 $ \v2 -> f v1 v2
