{-# language FlexibleContexts #-}
{-# language TypeOperators #-}

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

import           Dhrun.Types.Cfg
import           Dhrun.Pure
import           Dhrun.Conduit
import           Protolude
import           System.Exit
                   ( ExitCode(..) )
import           Data.Conduit
                   ( ConduitT )
import qualified System.Posix.Signals                              as SPS
                   ( installHandler
                   , keyboardSignal
                   , Handler(..)
                   )
import qualified Data.Conduit.Process.Typed                        as PT
import qualified Data.Conduit.Binary                               as CB
import qualified Data.Text                                         as T
                   ( isInfixOf )
import qualified System.IO                                         as SIO
                   ( BufferMode(NoBuffering)
                   , hSetBuffering
                   , withBinaryFile
                   , IOMode(WriteMode)
                   , stdout
                   )

import           Control.Monad.IO.Unlift
                   ( MonadIO(..)
                   , withRunInIO
                   )
import           Control.Monad.Writer
                   ( MonadWriter
                   , tell
                   , runWriterT
                   )
import qualified System.Directory                                  as SD
                   ( createDirectoryIfMissing )
import qualified System.Environment                                as SE
                   ( getEnvironment )
import qualified System.Timeout                                    as ST

import           System.Console.ANSI

putC :: MonadIO m => Text -> Color -> m ()
putC content color = setC color *> putT content *> setC White
 where
  setC c = liftIO $ setSGR [SetColor Foreground Dull c]
  putT :: MonadIO m => Text -> m ()
  putT = putStr

-- | runDhrun d runs a dhrun specification in the lifted IO monad.
runDhrun :: (MonadIO m) => Cfg -> m ()
runDhrun dhallExec =
  runWriterT (runReaderT runAllSteps dhallExec) <&> snd >>= liftIO . \case
    [] -> putC "Success. " Green
      <> putText "No errors were encountered and all requirements were met."
    errors ->
      putText "Error log:"
        <> for_ errors Protolude.putText
        <> putC "Failure. " Red
        <> die "exiting."

runAllSteps :: (MonadIO m, MonadReader Cfg m, MonadWriter [Text] m) => m ()
runAllSteps = do
  liftIO $ SIO.hSetBuffering SIO.stdout SIO.NoBuffering
  runWorkDir -- | setting up the working directory
  runPre     -- | preprocessing steps
  runAsyncs  -- | running the main async step
  runChecks  -- | running file checks
  runPost    -- | postprocessing steps
 where
  runPre  = runMultipleV ((toS <$>) <$> pre) "pre-processing"
  runPost = runMultipleV ((toS <$>) <$> post) "post-processing"

putV :: (MonadIO m, MonadReader Cfg m) => Text -> m ()
putV text =
  (== Verbose) . verbosity <$> ask >>= flip when (liftIO $ putText text)

runWorkDir :: (MonadIO m, MonadReader Cfg m) => m ()
runWorkDir = do
  (shouldRemove, wd) <- ask <&> \a -> (Remove == cleaning a, toS $ workdir a)
  when shouldRemove
    $  PT.runProcess_
    $  PT.shell
    $  toS
    $  ("rm -rf " :: Text)
    <> toS wd
  liftIO $ SD.createDirectoryIfMissing False wd

runChecks :: (MonadIO m, MonadReader Cfg m, MonadWriter [Text] m) => m ()
runChecks = (cmds <$> ask) >>= \case
  [] -> pu "no processes"
  l  -> do
    pu "start"
    for_ l $ \Cmd {..} -> do
      pu $ "running post-mortem checks for " <> toS name <> " " <> mconcat
        (intersperse " " (toS <$> args))
      for_ postchecks $ \FileCheck {..} -> do
        contents <- liftIO $ readFile (toS filename)
        forM_ (wants filecheck) $ \(Pattern x) -> unless
          (T.isInfixOf x contents)
          (tell
            [ "post-mortem check fail: "
              <> x
              <> " not found in file "
              <> toS filename
            ]
          )
        forM_ (avoids filecheck) $ \(Pattern x) -> when
          (T.isInfixOf x contents)
          (tell
            [ "post-mortem check fail: "
              <> x
              <> " found in file "
              <> toS filename
            ]
          )
    pu " done"
  where pu t = putV $ "check step:" <> t

runAsyncs :: (MonadIO m, MonadReader Cfg m, MonadWriter [Text] m) => m ()
runAsyncs = (cmds <$> ask) >>= \case
  [] -> pu "async step: no processes."
  l  -> do
    pu "async step: start"
    wd        <- workdir <$> ask
    externEnv <- liftIO SE.getEnvironment <&> (mapTuple toS <$>)
    asyncs    <- liftIO $ mkAsyncs l externEnv wd
    _         <- kbInstallHandler $ for_ asyncs cancel
    pu "async step: started processes"
    liftIO (waitAnyCancel asyncs)
      <&> snd
      <&> concludeCmd (any noWants l)
      >>= either tell putText
    pu "async step: done"
 where
  pu t = putV $ "async step:" <> t
  mkAsyncs :: [Cmd] -> [(Text, Text)] -> WorkDir -> IO [Async CmdResult]
  mkAsyncs l externEnv wd = for l (async . runCmd externEnv wd)
  kbInstallHandler :: (MonadIO m) => IO () -> m SPS.Handler
  kbInstallHandler h =
    liftIO $ SPS.installHandler SPS.keyboardSignal (SPS.Catch h) Nothing

runMultipleV
  :: (MonadIO m, MonadReader Cfg m) => (Cfg -> [Text]) -> Text -> m ()
runMultipleV getter desc = getter <$> ask >>= \case
  [] -> pu $ "no " <> desc <> " commands to run."
  l  -> do
    pu $ desc <> ": start"
    wdirFP <- ask <&> workdir <&> \(WorkDir wdt) -> wdt
    for_ l $ runOne wdirFP
    pu $ desc <> ": done"
 where
  pu t = putV $ desc <> " step:" <> t
  runOne wdirFP x =
    liftIO (PT.runProcess $ PT.setWorkingDir (toS wdirFP) (PT.shell $ toS x))
      >>= \case
            ExitSuccess -> pu ("ran one " <> desc <> " command.")
            ExitFailure _ ->
              liftIO
                $  die
                $  "failed to execute one "
                <> desc
                <> " command."
                <> x

-- | Runs a single command. should be the only 'complex' function in this codebaseaside from runAsyncs. TODO: add the exitcode check functionality.
runCmd :: [(Text, Text)] -> WorkDir -> Cmd -> IO CmdResult
runCmd fullExternEnv (WorkDir wd) c@Cmd {..} = do
  for_ otherwd $ \(WorkDir swd) -> SD.createDirectoryIfMissing True (toS swd)
  fromMaybe (Timeout c) <$> maybeTimeout go timeout
 where
  realwd = maybe (toS wd) toS otherwd

  -- | output files
  outFile :: FilePath
  outFile = getWdFilename realwd out
  errFile :: FilePath
  errFile = getWdFilename realwd err

  -- | process configuration
  pc :: PC
  pc =
    PT.proc (toS name) (toS <$> args)
      & PT.setWorkingDir (toS realwd)
      & PT.setEnv (mapTuple toS <$> envVars vars passvars fullExternEnv)
      & PT.setStdout PT.createSource
      & PT.setStderr PT.createSource
      & PT.setStdin PT.closed

  -- | lambda-scoped resource acquisition: stdin/out conduits and starting the process
  go :: IO CmdResult
  go = with3 (withSinkFileNoBuffering outFile)
             (withSinkFileNoBuffering errFile)
             (withWrappedSafeP c pc)
             logic

  -- | the main logic, that uses two sinks and the started process to
  -- do the streaming monitoring.
  logic :: Sink -> Sink -> P -> IO CmdResult
  logic sout serr p =
    finalizeCmd c
      <$> with2 (withAsync $ monitor (filecheck out) (PT.getStdout p) sout)
                (withAsync $ monitor (filecheck err) (PT.getStderr p) serr)
                waitEitherCatchCancel
      <*> (PT.getExitCode p >>= \case
            Nothing -> Killed <$ PT.stopProcess p
            Just ec -> return $ Died ec
          )

-- | May timeout an IO command.
maybeTimeout :: IO a -> Maybe Int -> IO (Maybe a)
maybeTimeout io = maybe (Just <$> io) (\t -> ST.timeout (1000000 * t) io)

-- | runs an IO action with a preconfigured process (PC) in lambda scope (P)
withWrappedSafeP :: Cmd -> PC -> (P -> IO CmdResult) -> IO CmdResult
withWrappedSafeP c pc f = catchIOE $ PT.withProcess pc f
 where
  catchIOE :: IO CmdResult -> IO CmdResult
  catchIOE ioOp =
    catch ioOp $ \(e :: IOException) -> return $ ThrewException c (show e)

-- | runs an IO action with a file sink in lambda scope
withSinkFileNoBuffering
  :: FilePath -> (ConduitT ByteString o IO () -> IO b) -> IO b
withSinkFileNoBuffering filepath lambdaIO =
  withRunInIO $ \run -> SIO.withBinaryFile filepath SIO.WriteMode $ \h -> do
    SIO.hSetBuffering h SIO.NoBuffering
    run $ lambdaIO $ CB.sinkHandle h
