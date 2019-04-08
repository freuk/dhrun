{-# language DerivingStrategies #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language ScopedTypeVariables #-}
{-# language TypeOperators #-}
{-# language NoImplicitPrelude #-}

{-# language OverloadedStrings #-}

{-|
Module      : Dhrun.Run
Description : runner
Copyright   : (c) Valentin Reis, 2018
License     : MIT
Maintainer  : fre@freux.fr
-}

module Dhrun.Run
  ( runDhrun
  )
where

import           Dhrun.Types
import           Protolude
import           System.Exit                    ( ExitCode(..) )
import           Data.Conduit
import           Data.Conduit.Combinators      as CC
import           System.Posix.Signals           ( installHandler
                                                , keyboardSignal
                                                , Handler(..)
                                                )
import           Data.Conduit.Process.Typed
import           Data.Conduit.Binary           as CB


-- | runDhrun d runs a dhrun specification in the lifted IO monad.
runDhrun :: (MonadIO m) => DhallExec -> m ()
runDhrun = runReaderT runAll

runAll :: (MonadIO m, MonadReader DhallExec m) => m ()
runAll = do
  runPre
  runAsyncs
  runChecks
  runPost

putV :: (MonadIO m, MonadReader DhallExec m) => Text -> m ()
putV text =
  (== Verbose) . verbosityLevel <$> ask >>= flip when (liftIO $ putText text)

runChecks :: (MonadIO m, MonadReader DhallExec m) => m ()
runChecks = (preCmds <$> ask) >>= \case
  [] -> putV "post-mortem check step: no processes."
  _  -> do
    putV "post-mortem checks: start"
    _ <- undefined
    putV "post-mortem checks: done"

runAsyncs :: (MonadIO m, MonadReader DhallExec m) => m ()
runAsyncs = (processSpecs <$> ask) >>= \case
  [] -> putV "async step: no processes."
  l  -> do
    putV "async step: start"
    asyncs <- liftIO $ for l (async . runCmd)
    _      <- liftIO $ kbInstallHandler $ for_ asyncs cancel
    putV "Processes started."
    _ <- liftIO $ waitAnyCancel asyncs
    putV "async step: done"

runPre :: (MonadIO m, MonadReader DhallExec m) => m ()
runPre = runMultipleV preCmds "pre-processing"

runPost :: (MonadIO m, MonadReader DhallExec m) => m ()
runPost = runMultipleV postCmds "post-processing"

runMultipleV
  :: (MonadIO m, MonadReader DhallExec m)
  => (DhallExec -> [Text])
  -> Text
  -> m ()
runMultipleV getter desc = getter <$> ask >>= \case
  [] -> putV $ "no " <> desc <> " commands to run."
  l  -> do
    putV $ desc <> ": start"
    for_ l $ \x -> liftIO (runProcess (shell $ toS x)) >>= \case
      ExitSuccess -> putV ("ran one " <> desc <> " command.")
      ExitFailure _ ->
        liftIO $ die $ "failed to execute one " <> desc <> " command." <> x
    putV $ desc <> ": done"

newtype MonitoringResult = PatternMatched Text deriving (Show, Typeable)
instance Exception MonitoringResult
data TracebackScan = WarningTraceback | Clean deriving (Show)

runCmd
  :: (MonadIO m)
  => Cmd
  -> m (Either MonitoringResult (ExitCode, TracebackScan, TracebackScan))
runCmd = liftIO . undefined

kbInstallHandler :: (MonadIO m) => IO () -> m Handler
kbInstallHandler h = liftIO $ installHandler keyboardSignal (Catch h) Nothing

withAsyncConduitsOnProcess
  :: Process () (ConduitT () ByteString IO ()) (ConduitT () ByteString IO ())
  -> ConduitT ByteString Void IO ()
  -> Check
  -> ConduitT ByteString Void IO ()
  -> Check
  -> (Async TracebackScan -> Async TracebackScan -> IO b)
  -> IO b
withAsyncConduitsOnProcess p outSink outTest errSink errTest = withAsyncs
  (doFilter outTest (getStdout p) outSink)
  (doFilter errTest (getStderr p) errSink)

withAsyncs :: IO a -> IO a1 -> (Async a -> Async a1 -> IO b) -> IO b
withAsyncs io1 io2 f =
  liftIO $ withAsync io1 $ \a1 -> withAsync io2 $ \a2 -> f a1 a2

doFilter
  :: (MonadIO m)
  => Check
  -> ConduitT () ByteString m ()
  -> ConduitT ByteString Void m ()
  -> m TracebackScan
doFilter behavior source sink =
  runConduit
    $              source
    .|             CB.lines
    .|             makeBehavior behavior
    `fuseUpstream` CC.unlinesAscii
    `fuseUpstream` sink

makeBehavior = Protolude.undefined

{-makeBehavior-}
  {-:: (MonadIO m)-}
  {-=> Check-}
  {--> ConduitT ByteString ByteString m TracebackScan-}
{-makeBehavior Check{..} = \case-}
  {-Just ExpectClean       -> warnOnTraceback False-}
  {-Just (WaitFor message) -> untilMatch message False-}
  {-Nothing                -> awaitForever yield $> Clean-}

--
-- warnOnTraceback
--   :: (MonadIO m) => Bool -> ConduitT ByteString ByteString m TracebackScan
-- warnOnTraceback sawTraceback = await >>= \case
--   Just b | B.isInfixOf "Traceback" b -> yield b >> warnOnTraceback True
--          | otherwise                 -> yield b >> warnOnTraceback sawTraceback
--   Nothing -> if sawTraceback then return WarningTraceback else return Clean
--

{-untilMatch-}
 {-:: (MonadIO m)-}
 {-=> Text-}
 {--> Bool-}
 {--> ConduitT ByteString ByteString m TracebackScan-}
{-untilMatch msg sawTraceback = await >>= \case-}
 {-Just b-}
   {-| B.isInfixOf "Traceback" b-}
   {--> untilMatch msg True >> yield b >> untilMatch msg True-}
   {-| B.isInfixOf (TE.encodeUtf8 msg) b && not sawTraceback-}
   {--> throw (PatternMatched $ TE.decodeUtf8 b)-}
   {-| otherwise-}
   {--> yield b >> untilMatch msg sawTraceback-}
 {-Nothing -> return Clean-}

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
--
--
--
--
--
