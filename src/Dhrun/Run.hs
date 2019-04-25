{-# language DerivingStrategies #-}
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

import           Dhrun.Types.Cfg
import           Dhrun.Pure
import           Dhrun.Conduit
import           Protolude
import           System.Exit                    ( ExitCode(..) )
import           Data.Conduit                   ( ConduitT
                                                , runConduit
                                                , fuseUpstream
                                                , (.|)
                                                )
import qualified Data.Conduit.Combinators      as CC
import qualified System.Posix.Signals          as SPS
                                                ( installHandler
                                                , keyboardSignal
                                                , Handler(..)
                                                )
import qualified Data.Conduit.Process.Typed    as PT
import qualified Data.Conduit.Binary           as CB
import qualified Data.Text                     as T
                                                ( isInfixOf
                                                , lines
                                                )
import qualified System.IO                     as SIO
                                                ( BufferMode(NoBuffering)
                                                , hSetBuffering
                                                , withBinaryFile
                                                , IOMode(WriteMode)
                                                , stdout
                                                )

import           Control.Monad.IO.Unlift        ( MonadIO(..)
                                                , withRunInIO
                                                )
import           Control.Monad.Writer           ( MonadWriter
                                                , tell
                                                , runWriterT
                                                )
import qualified System.Directory              as SD
                                                ( createDirectoryIfMissing )
import qualified System.Environment            as SE
                                                ( getEnvironment )
import qualified System.Timeout                as ST

import           System.Console.ANSI

putC :: MonadIO m => Text -> Color -> m ()
putC content color = setC color *> putT content *> setC White
  where setC c = liftIO $ setSGR [SetColor Foreground Dull c]

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


data ProcessWas = Died ExitCode | Killed

data Std = Out | Err deriving (Show)

data ConduitSpec = ConduitSpec {
  conduit :: ConduitT ByteString Void IO (),
  ccheck   :: Check
}


stdToS :: Std -> Text
stdToS Out = "stdout"
stdToS Err = "stderr"

putT :: MonadIO m => Text -> m ()
putT = putStr

-- | runDhrun d runs a dhrun specification in the lifted IO monad.
runDhrun :: (MonadIO m) => Cfg -> m ()
runDhrun dhallExec = runWriterT (runReaderT runAll dhallExec) >>= \case
  ((), []) -> liftIO $ putC "Success. " Green *> putText
    "No errors were encountered and all requirements were met."
  ((), errors) -> liftIO $ do
    putText "Error log:"
    for_ errors Protolude.putText
    putC "Failure. " Red
    die "exiting."

runAll :: (MonadIO m, MonadReader Cfg m, MonadWriter [Text] m) => m ()
runAll = do
  liftIO $ SIO.hSetBuffering SIO.stdout SIO.NoBuffering
  runWorkDir -- setting up the working directory
  runPre     -- preprocessing steps
  runAsyncs  -- running the main async step
  runChecks  -- running file checks
  runPost    -- postprocessing steps
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
  [] -> putV "post-mortem check step: no processes."
  l  -> do
    putV "post-mortem checks: start"
    for_ l $ \Cmd {..} -> do
      putV $ "running post-mortem checks for " <> toS name <> " " <> mconcat
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
        tell ["something"]
    putV "post-mortem checks: done"

runAsyncs :: (MonadIO m, MonadReader Cfg m, MonadWriter [Text] m) => m ()
runAsyncs = (cmds <$> ask) >>= \case
  [] -> putV "async step: no processes."
  l  -> do
    putV "async step: start"
    wd <- workdir <$> ask
    let haveWants = null $ mconcat (wants . filecheck . out <$> l) ++ mconcat
          (wants . filecheck . err <$> l)
    externEnv <- (\lenv -> (\(n, v) -> (toS n, toS v)) <$> lenv)
      <$> liftIO SE.getEnvironment
    asyncs <- liftIO $ mkAsyncs l externEnv wd
    _      <- liftIO $ kbInstallHandler $ for_ asyncs cancel
    putV "processes started."
    snd <$> liftIO (waitAnyCancel asyncs) >>= conclude haveWants
    putV "async step: done"
 where
  mkAsyncs :: [Cmd] -> [(Text, Text)] -> WorkDir -> IO [Async CmdResult]
  mkAsyncs l externEnv wd = for l (async . runCmd externEnv wd)

conclude :: (MonadWriter [Text] m, MonadIO m) => Bool -> CmdResult -> m ()
conclude _ (Timeout c) =
  tell $ "The following command timed out:" : T.lines (toS $ encodeCmd c)
conclude _ (DiedFailure c n) =
  tell
    $  "The following command died with exit code "
    <> show n
    <> " :"
    :  T.lines (toS $ encodeCmd c)
conclude True  (DiedLegal _) = putText "All commands exited successfully."
conclude False (DiedLegal c) = tell
  [ "command exited:"
      <> mconcat (intersperse "\n" (T.lines (toS $ encodeCmd c)))
  ]
conclude _ (FoundAll c) =
  putText
    $ "All searched patterns in the following command were found. Killing all processes.\n "
    <> mconcat (intersperse "\n" (T.lines (toS $ encodeCmd c)))
conclude _ (FoundIllegal c t e) =
  tell
    $  "The illegal pattern "
    <> t
    <> " was found in the output of this process' "
    <> stdToS e
    <> ":"
    :  T.lines (toS $ encodeCmd c)
conclude _ (OutputLacking c e) =
  tell
    $  "This process' "
    <> stdToS e
    <> " was found to be lacking pattern(s):"
    :  T.lines (toS $ encodeCmd c)
conclude _ (ConduitException c e) =
  tell $ "This process ended with a conduit exception:" <> stdToS e : T.lines
    (toS $ encodeCmd c)
conclude _ (ThrewException c e) =
  tell $ "This process' execution ended with an exception: " <> show e : T.lines
    (toS $ encodeCmd c)

runMultipleV
  :: (MonadIO m, MonadReader Cfg m) => (Cfg -> [Text]) -> Text -> m ()
runMultipleV getter desc = getter <$> ask >>= \case
  [] -> putV $ "no " <> desc <> " commands to run."
  l  -> do
    putV $ desc <> ": start"
    wdirFP <- ask <&> workdir <&> \(WorkDir wdt) -> wdt
    for_ l $ runOne wdirFP
    putV $ desc <> ": done"
 where
  runOne wdirFP x =
    liftIO (PT.runProcess $ PT.setWorkingDir (toS wdirFP) (PT.shell $ toS x))
      >>= \case
            ExitSuccess -> putV ("ran one " <> desc <> " command.")
            ExitFailure _ ->
              liftIO
                $  die
                $  "failed to execute one "
                <> desc
                <> " command."
                <> x

type P  = PT.Process
  () (ConduitT () ByteString IO ()) (ConduitT () ByteString IO ())
type PC = PT.ProcessConfig
  () (ConduitT () ByteString IO ()) (ConduitT () ByteString IO ())

maybeM :: Monad m => m b -> (a -> m b) -> m (Maybe a) -> m b
maybeM n j x = maybe n j =<< x

runCmd :: [(Text, Text)] -> WorkDir -> Cmd -> IO CmdResult
runCmd fullExternEnv (WorkDir wd) c@Cmd {..} =
  fromMaybe (Timeout c) <$> maybeTimeout timeout io
 where
  io = withSinkFileNoBuffering (getStringWD out) $ \outSink ->
    withSinkFileNoBuffering (getStringWD err) $ \errSink ->
      ( catch (PT.withProcess pc (go outSink errSink))
      $ \(e :: IOException) -> return $ ThrewException c e
      )
  getStringWD = toS . getfn wd
  go
    :: ConduitT ByteString Void IO ()
    -> ConduitT ByteString Void IO ()
    -> P
    -> IO CmdResult
  go outSink errSink p =
    finalize c
      <$> withAsyncConduitsOnProcess
            p
            ConduitSpec {conduit = outSink, ccheck = filecheck out}
            ConduitSpec {conduit = errSink, ccheck = filecheck err}
            waitEitherCatchCancel
      <*> maybeM (PT.stopProcess p <&> const Killed)
                 (return . Died)
                 (PT.getExitCode p)

  pc :: PC
  pc =
    PT.proc (toS name) (toS <$> args)
      & PT.setWorkingDir (toS wd)
      & PT.setEnv (mapTuple toS <$> envVars vars passvars fullExternEnv)
      & PT.setStdout PT.createSource
      & PT.setStderr PT.createSource
      & PT.setStdin PT.closed

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

noChecks :: Cmd -> Bool
noChecks Cmd {..} =
  null (wants $ filecheck out) && null (wants $ filecheck err)

getfn :: Text -> FileCheck Check -> Text
getfn wd fc = wd <> "/" <> toS (filename fc)

maybeTimeout :: Maybe Int -> IO a -> IO (Maybe a)
maybeTimeout i io = case i of
  Nothing -> Just <$> io
  Just t  -> ST.timeout (100000 * t) io

kbInstallHandler :: (MonadIO m) => IO () -> m SPS.Handler
kbInstallHandler h =
  liftIO $ SPS.installHandler SPS.keyboardSignal (SPS.Catch h) Nothing

-- | withConduitSinks runs an IO action. It gives two conduit testing asyncs in scope.
-- | THROWS PatternMatched
withAsyncConduitsOnProcess
  :: PT.Process () (ConduitT () ByteString IO ()) (ConduitT () ByteString IO ())
  -> ConduitSpec -- stdout conduit spec
  -> ConduitSpec -- stderr conduit spec
  -> (Async () -> Async () -> IO b) -- the lambda-wrapped IO action
  -> IO b
withAsyncConduitsOnProcess p cOut cErr = withAsyncs
  (doFilter (ccheck cOut) (PT.getStdout p) (conduit cOut))
  (doFilter (ccheck cErr) (PT.getStderr p) (conduit cErr))

withAsyncs :: IO () -> IO () -> (Async () -> Async () -> IO b) -> IO b
withAsyncs io1 io2 f =
  liftIO $ withAsync io1 $ \a1 -> withAsync io2 $ \a2 -> f a1 a2

-- | THROWS PatternMatched
doFilter
  :: Check
  -> ConduitT () ByteString IO ()
  -> ConduitT ByteString Void IO ()
  -> IO ()
doFilter behavior source sink =
  runConduit
    $              source
    .|             CB.lines
    .|             makeBehavior behavior
    `fuseUpstream` CC.unlinesAscii
    `fuseUpstream` sink

-- | runs an IO action with a file sink in lambda scope
withSinkFileNoBuffering
  :: FilePath -> (ConduitT ByteString o IO () -> IO b) -> IO b
withSinkFileNoBuffering filepath lambdaIO =
  withRunInIO $ \run -> SIO.withBinaryFile filepath SIO.WriteMode $ \h -> do
    SIO.hSetBuffering h SIO.NoBuffering
    run $ lambdaIO $ CB.sinkHandle h
