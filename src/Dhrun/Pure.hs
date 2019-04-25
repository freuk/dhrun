{-# language RecordWildCards #-}
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
  , concludeCmd
  , mapTuple
  , finalizeCmd
  , getWdFilename
  , stdToS
  , noWants
  , with3
  , with2
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
import qualified Data.Text                     as T
                                                ( lines )
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

concludeCmd :: Bool -> CmdResult -> Either [Text] Text
concludeCmd True  (DiedLegal _) = Right "All commands exited successfully."
concludeCmd False (DiedLegal c) = Left
  [ "command exited:"
      <> mconcat (intersperse "\n" (T.lines (toS $ encodeCmd c)))
  ]
concludeCmd _ (Timeout c) =
  Left $ "The following command timed out:" : T.lines (toS $ encodeCmd c)
concludeCmd _ (DiedFailure c n) =
  Left
    $  "The following command died with exit code "
    <> show n
    <> " :"
    :  T.lines (toS $ encodeCmd c)
concludeCmd _ (FoundAll c) = Right
  ("All searched patterns in the following command were found. Killing all processes.\n "
  <> mconcat (intersperse "\n" (T.lines (toS $ encodeCmd c)))
  )
concludeCmd _ (FoundIllegal c t e) =
  Left
    $  "The illegal pattern "
    <> t
    <> " was found in the output of this process' "
    <> stdToS e
    <> ":"
    :  T.lines (toS $ encodeCmd c)
concludeCmd _ (OutputLacking c e) =
  Left
    $  "This process' "
    <> stdToS e
    <> " was found to be lacking pattern(s):"
    :  T.lines (toS $ encodeCmd c)
concludeCmd _ (ConduitException c e) =
  Left $ "This process ended with a conduit exception:" <> stdToS e : T.lines
    (toS $ encodeCmd c)
concludeCmd _ (ThrewException c e) =
  Left $ "This process' execution ended with an exception: " <> show e : T.lines
    (toS $ encodeCmd c)

stdToS :: Std -> Text
stdToS Out = "stdout"
stdToS Err = "stderr"

noWants :: Cmd -> Bool
noWants Cmd {..} =
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

getWdFilename :: Text -> FileCheck a -> FilePath
getWdFilename wdT fc = toS $ wdT <> "/" <> toS (filename fc)

finalizeCmd
  :: Cmd
  -> Either (Either SomeException ()) (Either SomeException ())
  -> ProcessWas
  -> CmdResult
finalizeCmd c _ (Died (ExitFailure n)) = DiedFailure c n
finalizeCmd c (Left (Right ())) _ =
  if noWants c then DiedLegal c else OutputLacking c Out
finalizeCmd c (Right (Right ())) _ =
  if noWants c then DiedLegal c else OutputLacking c Err
finalizeCmd c (Left (Left e)) _ = case fromException e of
  Just (ThrowFoundAnAvoid t) -> FoundIllegal c t Out
  Just ThrowFoundAllWants    -> FoundAll c
  Nothing                    -> ConduitException c Out
finalizeCmd c (Right (Left e)) _ = case fromException e of
  Just (ThrowFoundAnAvoid t) -> FoundIllegal c t Err
  Just ThrowFoundAllWants    -> FoundAll c
  Nothing                    -> ConduitException c Err

with3
  :: ((t1 -> t2) -> t3)
  -> ((t4 -> t5) -> t2)
  -> ((t6 -> t7) -> t5)
  -> (t1 -> t4 -> t6 -> t7)
  -> t3
with3 w1 w2 w3 f = w1 $ \v1 -> w2 $ \v2 -> w3 $ \v3 -> f v1 v2 v3

with2 :: ((t1 -> t2) -> t3) -> ((t4 -> t5) -> t2) -> (t1 -> t4 -> t5) -> t3
with2 w1 w2 f = w1 $ \v1 -> w2 $ \v2 -> f v1 v2
