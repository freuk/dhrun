{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-|
Module      : Dhrun.Run
Description : runner
Copyright   : (c) Valentin Reis, 2018
License     : MIT
Maintainer  : fre@freux.fr
-}

module Dhrun.Conduit
  ( monitor
  , Sink
  , Source
  , P
  , PC
  )
where

import           Dhrun.Types.Cfg
import           Dhrun.Pure
import           Protolude
import qualified Data.ByteString                                   as B
import           Control.Exception.Base
                   ( throw )
import qualified Data.Conduit.Binary                               as CB
                   ( lines )
import qualified Data.Conduit.Combinators                          as CC
                   ( unlinesAscii )
import           Data.Conduit
                   ( ConduitT
                   , yield
                   , await
                   , runConduit
                   , (.|)
                   )
import qualified Data.Conduit.Process.Typed                        as PT
                   ( Process
                   , ProcessConfig
                   )
import qualified Data.Conduit.Lift                                 as CLF
import           Data.Time.Clock.POSIX

type Source = ConduitT () ByteString IO ()
type Sink = ConduitT ByteString Void IO ()
type P  = PT.Process () Source Source
type PC = PT.ProcessConfig () Source Source

-- | output monitoring conduit. THROWS PatternMatched - with a one second wait to
monitor
  :: Check
  -> ConduitT () ByteString IO ()
  -> ConduitT ByteString Void IO ()
  -> IO ()
monitor behavior source sink =
  runConduit
    $  source
    .| CB.lines
    .| void (CLF.runStateC initialState (CLF.runReaderC behavior makeBehavior))
    .| CC.unlinesAscii
    .| sink

initialState :: MonitoringState
initialState = Nothing
type MonitoringState = Maybe Matched
data Matched = Matched
  { matched :: Text
  , atTime :: POSIXTime } deriving (Show)

-- | makeBehavior builds an IO conduit that throws a PatternMatched when
-- all wanted pattern or one avoided pattern are found
makeBehavior
  :: (MonadIO m, MonadState MonitoringState m, MonadReader Check m)
  => ConduitT ByteString ByteString m ()
makeBehavior = wants <$> ask >>= \case
  [] -> cleanLooper
  as -> expectfulLooper $ toS <$> as

expectfulLooper
  :: (MonadIO m, MonadState MonitoringState m, MonadReader Check m)
  => [ByteString]
  -> ConduitT ByteString ByteString m ()
expectfulLooper [] = throw ThrowFoundAllWants
expectfulLooper l  = noAvoidsAndOtherwise $ \b ->
  yield b >> expectfulLooper (filter (not . flip B.isInfixOf b) (toS <$> l))

cleanLooper
  :: (MonadIO m, MonadState MonitoringState m, MonadReader Check m)
  => ConduitT ByteString ByteString m ()
cleanLooper = noAvoidsAndOtherwise $ \b -> yield b >> cleanLooper

noAvoidsAndOtherwise
  :: (MonadIO m, MonadState MonitoringState m, MonadReader Check m)
  => (ByteString -> ConduitT ByteString ByteString m ())
  -> ConduitT ByteString ByteString m ()
noAvoidsAndOtherwise otherwiseConduit = do
  av <- avoids <$> ask
  t  <- liftIO getPOSIXTime
  get >>= \case
    Just m  -> goThrow t m
    Nothing -> return ()
  await >>= \case
    Just b -> case filter (`B.isInfixOf` b) (toS <$> av) of
      []     -> otherwiseConduit b
      xh : _ -> do
        put . Just $ Matched (toS xh) t
        otherwiseConduit b
    Nothing -> return ()
 where
  goThrow :: POSIXTime -> Matched -> ConduitT ByteString ByteString m ()
  goThrow t Matched {..} =
    when (t > 1 + atTime) (throw $ ThrowFoundAnAvoid matched)

{-noAvoidsAndOtherwise-}
  {-:: (MonadIO m)-}
  {-=> Maybe (Integer, a)-}
  {--> Check-}
  {--> (Maybe (Integer, a) -> ByteString -> ConduitT ByteString ByteString m ())-}
  {--> ConduitT ByteString ByteString m ()-}
{-noAvoidsAndOtherwise mt Check {..} otherwiseConduit = do-}
  {-t <- liftIO askPOSIXTime <&> toInteger-}
  {-case shouldThrow t mt of-}
    {-Just xh -> throw $ ThrowFoundAnAvoid $ toS xh-}
    {-Nothing -> await >>= \case-}
      {-Just b -> case filter (`B.isInfixOf` b) (toS <$> avoids) of-}
        {-[]     -> otherwiseConduit t b-}
        {-xh : _ -> otherwiseConduit t b-}
      {-Nothing -> return ()-}
