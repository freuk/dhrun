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
import qualified Data.Conduit.Combinators      as CC
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
import qualified Data.ByteString               as B
import           Control.Exception.Base         ( Exception
                                                , try
                                                , throw
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
    putV "TODO check that result value bitch" -- TODO
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

data MonitoringResult = PatternMatched deriving (Show, Typeable)
instance Exception MonitoringResult
data TracebackScan = WarningTraceback Text | Clean deriving (Show)

runCmd
  :: Text -- | workDir
  -> Cmd
  -> IO (Either MonitoringResult (ExitCode, TracebackScan, TracebackScan))
runCmd wd Cmd {..} =
  try $ withConduitSinks (getfn out) (getfn err) $ \outSink errSink ->
    withProcess pc $ \p ->
      withAsyncConduitsOnProcess
          p
          ConduitSpec {conduit = outSink, ccheck = filecheck out}
          ConduitSpec {conduit = errSink, ccheck = filecheck err}
          waitEither
        >>= \case
              Left  Clean -> (, Clean, Clean) <$> waitExitCode p
              Right Clean -> (, Clean, Clean) <$> waitExitCode p
              Right (WarningTraceback t) ->
                (, Clean, WarningTraceback t) <$> waitExitCode p
              Left (WarningTraceback t) ->
                (, (WarningTraceback t), Clean) <$> waitExitCode p
 where
  pc = configureConduits wd $ proc (toS name) (toS <$> args)
  getfn :: FileCheck Check -> Text
  getfn fc = wd <> "/" <> filename fc

kbInstallHandler :: (MonadIO m) => IO () -> m Handler
kbInstallHandler h = liftIO $ installHandler keyboardSignal (Catch h) Nothing

data ConduitSpec = ConduitSpec {
  conduit :: ConduitT ByteString Void IO (),
  ccheck   :: Check
}

-- | withConduitSinks runs an IO action. It gives two conduit testing asyncs in scope.
withAsyncConduitsOnProcess
  :: Process () (ConduitT () ByteString IO ()) (ConduitT () ByteString IO ())
  -> ConduitSpec --stdout conduit spec
  -> ConduitSpec --stderr conduit spec
  -> (Async TracebackScan -> Async TracebackScan -> IO b) -- the lambda-wrapped IO action
  -> IO b
withAsyncConduitsOnProcess p cOut cErr = withAsyncs
  (doFilter (ccheck cOut) (getStdout p) (conduit cOut))
  (doFilter (ccheck cErr) (getStderr p) (conduit cErr))
 where
  withAsyncs
    :: IO TracebackScan
    -> IO TracebackScan
    -> (Async TracebackScan -> Async TracebackScan -> IO b)
    -> IO b
  withAsyncs io1 io2 f =
    liftIO $ withAsync io1 $ \a1 -> withAsync io2 $ \a2 -> f a1 a2

doFilter
  :: Check
  -> ConduitT () ByteString IO ()
  -> ConduitT ByteString Void IO ()
  -> IO TracebackScan
doFilter behavior source sink =
  runConduit
    $              source
    .|             CB.lines
    .|             makeBehavior behavior
    `fuseUpstream` CC.unlinesAscii
    `fuseUpstream` sink

makeBehavior :: Check -> ConduitT ByteString ByteString IO TracebackScan
makeBehavior Check {..} = case wants of
  [] -> cleanLooper
  as -> expectfulLooper $ toS <$> as
 where

  cleanLooper :: ConduitT ByteString ByteString IO TracebackScan
  cleanLooper = noAvoidsAndOtherwise $ \b -> yield b >> cleanLooper

  expectfulLooper
    :: [ByteString] -> ConduitT ByteString ByteString IO TracebackScan
  expectfulLooper [] = throw PatternMatched
  expectfulLooper l  = noAvoidsAndOtherwise $ \b -> yield b
    >> expectfulLooper (filter (\x -> not $ B.isInfixOf x b) (toS <$> l))

  noAvoidsAndOtherwise otherwiseConduit = await >>= \case
    Just b | any (`B.isInfixOf` b) (toS <$> avoids) -> throw PatternMatched
           | otherwise                              -> otherwiseConduit b
    Nothing -> return Clean

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

-- | runs an IO action with two conduits in lambda scope
withConduitSinks
  :: Text -- stdout filename
  -> Text -- stderr filename
  -> (ConduitT ByteString o IO () -> ConduitT ByteString o1 IO () -> IO b)
  -> IO b
withConduitSinks outName errName lambdaIO =
  withSinkFileNoBuffering (toS outName) $ \outSink ->
    withSinkFileNoBuffering (toS errName) $ \errSink -> lambdaIO outSink errSink

-- | runs an IO action with a file sink in lambda scope
withSinkFileNoBuffering
  :: FilePath -> (ConduitT ByteString o IO () -> IO b) -> IO b
withSinkFileNoBuffering filepath lambdaIO =
  withRunInIO $ \run -> SIO.withBinaryFile filepath SIO.WriteMode $ \h -> do
    SIO.hSetBuffering h SIO.NoBuffering
    run $ lambdaIO $ sinkHandle h
