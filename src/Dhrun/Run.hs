{-# language DerivingStrategies #-}
{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language ScopedTypeVariables #-}
{-# language TypeOperators #-}
{-# language NoImplicitPrelude #-}
{-# language RecordWildCards #-}

{-|
Module      : Dhrun.Run
Description : runner
Copyright   : (c) Valentin Reis, 2018
License     : MIT
Maintainer  : fre@freux.fr
-}

module Dhrun.Run
  ()
where

import           Dhrun.Types
import           Protolude
import           System.Exit                    ( ExitCode(..) )
import           Data.ByteString               as B
                                         hiding ( empty )
import           Data.Conduit
import           Data.Conduit.Combinators      as CC
import           System.Posix.Signals           ( installHandler
                                                , keyboardSignal
                                                , Handler(..)
                                                )
import           System.Directory

import           Control.Exception.Base         ( Exception
                                                , try
                                                , throw
                                                )
import           Control.Monad.IO.Unlift        ( MonadIO(..)
                                                , withRunInIO
                                                )
import           Data.Text                     as T
                                                ( unpack
                                                , Text
                                                )
import           Data.Text.Encoding            as TE
                                                ( decodeUtf8
                                                , encodeUtf8
                                                )
import           Data.Typeable                  ( Typeable )
import           System.IO                      ( BufferMode(NoBuffering)
                                                , hSetBuffering
                                                )
import qualified System.IO                     as IO
import           Data.Conduit.Process.Typed
import           Data.Conduit.Binary           as CB

-- | runDhrun d runs a dhrun specification in the lifted IO monad.
runDhrun :: (MonadIO m, Monoid (m ())) => DhallExec -> m ()
runDhrun dex@DhallExec {..} = mconcat
  [ putVerbose "running pre-processing step"
  , runPre
  , runAsyncs
  , runChecks
  , runPost
  ]
 where
  putVerbose = when (verbosity == Verbose) putText
  runPre     = undefined
  runPost    = undefined
  runAsyncs  = undefined
  runChecks  = undefined

newtype MonitoringResult = PatternMatched Text deriving (Show, Typeable)
instance Exception MonitoringResult
data TracebackScan = WarningTraceback | Clean deriving (Show)
runCmd
  :: (MonadIO m)
  => Cmd
  -> m (Either MonitoringResult (ExitCode, TracebackScan, TracebackScan))
runCmd = Protolude.undefined

-- myWhich :: Text -> IO Text
-- myWhich str =
--   (toS <$> readProcessStdout_ (shell $ toS $ "which " <> str)) >>= \case
--     "" -> die $ "Argo `" <> str <> "` not in $PATH."
--     p  -> printInfo ("Found " <> str <> " at " <> p) $> p
-- 
-- sudoRemoveFile :: (Text -> IO ()) -> Text -> Text -> IO ()
-- sudoRemoveFile printer desc filePath = do
--   foundSocket <- doesFileExist $ toS filePath
--   when foundSocket $ go False
--   printInfo $ "OK: " <> desc <> " " <> filePath
--  where
--   go useSudo = do
--     printer $ "found stale " <> desc <> " at " <> filePath <> ".. "
--     runProcess
--         ( shell
--         $ toS ((if useSudo then "sudo " else "") <> "rm -rf " <> filePath)
--         )
--       >>= \case
--             ExitSuccess ->
--               colorShell Green $ putText " Successfully removed.\n"
--             ExitFailure _ -> if useSudo
--               then printer
--                 ("Failed to remove stale " <> desc <> ", even with sudo.")
--               else do
--                 printer
--                   ("Failed to remove stale " <> desc <> ". Trying sudo..\n")
--                 go True
-- 
-- cleanSocket :: Text -> IO ()
-- cleanSocket = sudoRemoveFile putText "socket"
-- 
-- kbInstallHandler :: IO () -> IO Handler
-- kbInstallHandler h = installHandler keyboardSignal (Catch h) Nothing
-- 
-- 
-- 
-- withAsyncConduitsOnProcess
--   :: Process () (ConduitT () ByteString IO ()) (ConduitT () ByteString IO ())
--   -> ConduitT ByteString Void IO ()
--   -> Maybe TextBehavior
--   -> ConduitT ByteString Void IO ()
--   -> Maybe TextBehavior
--   -> (Async TracebackScan -> Async TracebackScan -> IO b)
--   -> IO b
-- withAsyncConduitsOnProcess p outSink outTest errSink errTest = withAsyncs
--   (doFilter outTest (getStdout p) outSink)
--   (doFilter errTest (getStderr p) errSink)
-- 
-- withAsyncs :: IO a -> IO a1 -> (Async a -> Async a1 -> IO b) -> IO b
-- withAsyncs io1 io2 f = withAsync io1 $ \a1 -> withAsync io2 $ \a2 -> f a1 a2
-- 
-- doFilter
--   :: (MonadIO m)
--   => Maybe TextBehavior
--   -> ConduitT () ByteString m ()
--   -> ConduitT ByteString Void m ()
--   -> m TracebackScan
-- doFilter behavior source sink =
--   runConduit
--     $              source
--     .|             CB.lines
--     .|             makeBehavior behavior
--     `fuseUpstream` CC.unlinesAscii
--     `fuseUpstream` sink
-- 
-- makeBehavior
--   :: (MonadIO m)
--   => Maybe TextBehavior
--   -> ConduitT ByteString ByteString m TracebackScan
-- makeBehavior = \case
--   Just ExpectClean       -> warnOnTraceback False
--   Just (WaitFor message) -> untilMatch message False
--   Nothing                -> awaitForever yield $> Clean
-- 
-- warnOnTraceback
--   :: (MonadIO m) => Bool -> ConduitT ByteString ByteString m TracebackScan
-- warnOnTraceback sawTraceback = await >>= \case
--   Just b | B.isInfixOf "Traceback" b -> yield b >> warnOnTraceback True
--          | otherwise                 -> yield b >> warnOnTraceback sawTraceback
--   Nothing -> if sawTraceback then return WarningTraceback else return Clean
-- 
-- untilMatch
--   :: (MonadIO m)
--   => Text
--   -> Bool
--   -> ConduitT ByteString ByteString m TracebackScan
-- untilMatch msg sawTraceback = await >>= \case
--   Just b
--     | B.isInfixOf "Traceback" b
--     -> untilMatch msg True >> yield b >> untilMatch msg True
--     | B.isInfixOf (TE.encodeUtf8 msg) b && not sawTraceback
--     -> throw (PatternMatched $ TE.decodeUtf8 b)
--     | otherwise
--     -> yield b >> untilMatch msg sawTraceback
--   Nothing -> return Clean
-- 
-- configureConduits
--   :: WorkingDirectory
--   -> ProcessConfig () () ()
--   -> ProcessConfig
--        ()
--        (ConduitM () ByteString IO ())
--        (ConduitM () ByteString IO ())
-- configureConduits (WorkingDirectory wd) p =
--   setStdout createSource
--     $ setStderr createSource
--     $ setStdin closed
--     $ setWorkingDir (toS wd) p
-- 
-- withConduitSinks
--   :: Text
--   -> Text
--   -> (ConduitT ByteString o IO () -> ConduitT ByteString o1 IO () -> IO b)
--   -> IO b
-- withConduitSinks outName errName f =
--   withSinkFileNoBuffering (T.unpack outName) $ \outSink ->
--     withSinkFileNoBuffering (T.unpack errName) $ \errSink -> f outSink errSink
-- 
-- withSinkFileNoBuffering
--   :: FilePath -> (ConduitT ByteString o IO () -> IO b) -> IO b
-- withSinkFileNoBuffering filepath inner =
--   withRunInIO $ \run -> IO.withBinaryFile filepath IO.WriteMode $ \h -> do
--     hSetBuffering h NoBuffering
--     run $ inner $ sinkHandle h
-- 
-- runI
--   :: WorkingDirectory
--   -> Instrumentation
--   -> IO (Either MonitoringResult (ExitCode, TracebackScan, TracebackScan))
-- runI workDir@(WorkingDirectory wd) (Instrumentation crProc (StdOutLog stdOut) (StdErrLog stdErr) t)
--   = try
--     $ withConduitSinks (wd <> "/" <> stdOut) (wd <> "/" <> stdErr)
--     $ \outSink errSink ->
--         withProcess (configureConduits workDir crProc) $ \p ->
--           withAsyncConduitsOnProcess p
--                                      outSink
--                                      outTest
--                                      errSink
--                                      errTest
--                                      waitEither
--             >>= \case
--                   Left  Clean -> (, Clean, Clean) <$> waitExitCode p
--                   Right Clean -> (, Clean, Clean) <$> waitExitCode p
--                   Right WarningTraceback ->
--                     (, Clean, WarningTraceback) <$> waitExitCode p
--                   Left WarningTraceback ->
--                     (, WarningTraceback, Clean) <$> waitExitCode p
--  where
--   outTest :: Maybe TextBehavior
--   errTest :: Maybe TextBehavior
--   (outTest, errTest) = case t of
--     Just (TestText (TextBehaviorStdout tOut) (TextBehaviorStderr tErr)) ->
--       (Just tOut, Just tErr)
--     Nothing -> (Nothing, Nothing)
-- 
-- processBehaviorToI
--   :: ProcessConfig () () () -> ProcessBehavior -> Maybe Instrumentation
-- processBehaviorToI crProc = \case
--   DontRun               -> Nothing
--   JustRun stdOut stdErr -> Just $ Instrumentation crProc stdOut stdErr Nothing
--   Test t stdOut stdErr  -> Just $ Instrumentation crProc stdOut stdErr (Just t)
