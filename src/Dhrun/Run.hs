{-# language DerivingStrategies #-}
{-# language TupleSections #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language RecordWildCards #-}
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
import           Control.Monad.Writer
import qualified Data.Text                     as T
                                                ( isInfixOf )
import qualified System.IO                     as SIO
                                                ( BufferMode(NoBuffering)
                                                , hSetBuffering
                                                , withBinaryFile
                                                , IOMode(WriteMode)
                                                )

import           Control.Monad.IO.Unlift        ( MonadIO(..)
                                                , withRunInIO
                                                )

-- | runDhrun d runs a dhrun specification in the lifted IO monad.
runDhrun :: (MonadIO m) => DhallExec -> m ()
runDhrun dhallExec = runWriterT (runReaderT runAll dhallExec) >>= \case
  ((), []) -> liftIO $ putText
    "Success. No errors were encountered and all requirements were met."
  ((), errors) -> liftIO $ do
    putText "Failure. The following errors were encountered:"
    for_ errors Protolude.print
    die "exiting."

runAll :: (MonadIO m, MonadReader DhallExec m, MonadWriter [Text] m) => m ()
runAll = do
  runPre
  runAsyncs
  runChecks
  runPost

putV :: (MonadIO m, MonadReader DhallExec m) => Text -> m ()
putV text =
  (== Verbose) . verbosity <$> ask >>= flip when (liftIO $ putText text)

runChecks :: (MonadIO m, MonadReader DhallExec m, MonadWriter [Text] m) => m ()
runChecks = (cmds <$> ask) >>= \case
  [] -> putV "post-mortem check step: no processes."
  l  -> do
    putV "post-mortem checks: start"
    for_ l $ \Cmd {..} -> do
      putV $ "running post-mortem checks for" <> name <> " " <> mconcat args
      for_ postchecks $ \FileCheck {..} -> do
        contents <- liftIO $ readFile (toS filename)
        forM_ (wants filecheck) $ \x -> unless
          (T.isInfixOf x contents)
          (tell
            [ "post-mortem check fail: "
              <> x
              <> " not found in file "
              <> filename
            ]
          )
        forM_ (avoids filecheck) $ \x -> when
          (T.isInfixOf x contents)
          (tell
            ["post-mortem check fail: " <> x <> " found in file " <> filename]
          )
        tell ["something"]
    putV "post-mortem checks: done"

runAsyncs :: (MonadIO m, MonadReader DhallExec m) => m ()
runAsyncs = (cmds <$> ask) >>= \case
  [] -> putV "async step: no processes."
  l  -> do
    putV "async step: start"
    wd     <- workdir <$> ask
    asyncs <- liftIO $ mkAsyncs l wd
    _      <- liftIO $ kbInstallHandler $ for_ asyncs cancel
    putV "Processes started."
    _ <- liftIO $ waitAnyCancel asyncs
    putV "async step: done"
 where
  mkAsyncs
    :: [Cmd]
    -> Text
    -> IO
         [ Async
             ( Either
                 MonitoringResult
                 (ExitCode, TracebackScan, TracebackScan)
             )
         ]
  mkAsyncs l wd = for l (async . runCmd wd)

runPre :: (MonadIO m, MonadReader DhallExec m) => m ()
runPre = runMultipleV pre "pre-processing"

runPost :: (MonadIO m, MonadReader DhallExec m) => m ()
runPost = runMultipleV post "post-processing"

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
  :: Text -- | workDir
  -> Cmd
  -> IO (Either MonitoringResult (ExitCode, TracebackScan, TracebackScan))
runCmd wd Cmd {..} =
  try
    $ withConduitSinks (wd <> "/" <> filename out) (wd <> "/" <> filename err)
    $ \outSink errSink -> withProcess pc $ \p ->
        withAsyncConduitsOnProcess p
                                   outSink
                                   (filecheck out)
                                   errSink
                                   (filecheck err)
                                   waitEither
          >>= \case
                Left  Clean -> (, Clean, Clean) <$> waitExitCode p
                Right Clean -> (, Clean, Clean) <$> waitExitCode p
                Right WarningTraceback ->
                  (, Clean, WarningTraceback) <$> waitExitCode p
                Left WarningTraceback ->
                  (, WarningTraceback, Clean) <$> waitExitCode p
  where pc = configureConduits wd $ proc (toS name) (toS <$> args)

kbInstallHandler :: (MonadIO m) => IO () -> m Handler
kbInstallHandler h = liftIO $ installHandler keyboardSignal (Catch h) Nothing

withAsyncConduitsOnProcess
  :: Process () (ConduitT () ByteString IO ()) (ConduitT () ByteString IO ())
  -> ConduitT ByteString Void IO () --stdout conduit
  -> Check --stdout check
  -> ConduitT ByteString Void IO () --stderr conduit
  -> Check --stderr check
  -> (Async TracebackScan -> Async TracebackScan -> IO b) --
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
 where
  makeBehavior :: Check -> ConduitT ByteString ByteString m TracebackScan
  makeBehavior _ = Protolude.undefined

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

configureConduits
  :: Text
  -> ProcessConfig () () ()
  -> ProcessConfig
       ()
       (ConduitM () ByteString IO ())
       (ConduitM () ByteString IO ())
configureConduits wd p =
  setStdout createSource
    $ setStderr createSource
    $ setStdin closed
    $ setWorkingDir (toS wd) p

withConduitSinks
  :: Text
  -> Text
  -> (ConduitT ByteString o IO () -> ConduitT ByteString o1 IO () -> IO b)
  -> IO b
withConduitSinks outName errName f =
  withSinkFileNoBuffering (toS outName) $ \outSink ->
    withSinkFileNoBuffering (toS errName) $ \errSink -> f outSink errSink

withSinkFileNoBuffering
  :: FilePath -> (ConduitT ByteString o IO () -> IO b) -> IO b
withSinkFileNoBuffering filepath inner =
  withRunInIO $ \run -> SIO.withBinaryFile filepath SIO.WriteMode $ \h -> do
    SIO.hSetBuffering h SIO.NoBuffering
    run $ inner $ sinkHandle h
