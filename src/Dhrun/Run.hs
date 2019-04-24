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

import           Dhrun.Internal
import           Dhrun.Pureutils
import           Protolude
import           System.Exit                    ( ExitCode(..) )
import           Data.Conduit                   ( ConduitT
                                                , runConduit
                                                , fuseUpstream
                                                , yield
                                                , await
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
import qualified Data.ByteString               as B
import           Control.Exception.Base         ( Exception
                                                , throw
                                                )
import qualified System.Directory              as SD
                                                ( createDirectoryIfMissing )
import qualified System.Environment            as SE
                                                ( getEnvironment )
import qualified System.Timeout                as ST

data CmdResult =
      Timeout Cmd
    | DiedLegal
    | DiedFailure Cmd Int
    | FoundAll Cmd
    | FoundIllegal Cmd Text Std
    | OutputLacking Cmd Std
    | ConduitException Cmd Std deriving (Show)

data MonitoringResult =
    ThrowFoundAllWants
  | ThrowFoundAnAvoid Text
  deriving (Show, Typeable)

instance Exception MonitoringResult

data ScanResult =  Clean | Lacking  deriving (Show)

data Std = Out | Err deriving (Show)
stdToS :: Std -> Text
stdToS Out = "stdout"
stdToS Err = "stderr"

{-btw :: (Functor f) => (t -> f b) -> t -> f t-}
{-btw k x = x <$ k x-}

{-(>>!) :: (Monad m) => m a -> (a -> m b) -> m a-}
{-(!<<) :: (Monad m) => (a -> m b) -> m a -> m a-}
{-k >>! x = k >>= btw x-}
{-(!<<) = flip (>>!)-}

{-infixr 7 >>!-}
{-infixl 0 !<<-}

{-btwc :: (Functor f) => f b -> b1 -> f b1-}
{-btwc m = btw (const m)-}

-- | runDhrun d runs a dhrun specification in the lifted IO monad.
runDhrun :: (MonadIO m) => Cfg -> m ()
runDhrun dhallExec = runWriterT (runReaderT runAll dhallExec) >>= \case
  ((), []) -> liftIO $ putText
    "Success. No errors were encountered and all requirements were met."
  ((), errors) -> liftIO $ do
    putText "Failure. Error log:"
    for_ errors Protolude.putText
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
  when shouldRemove $ PT.runProcess_ $ PT.shell $ toS $ ("rm -rf "::Text) <> toS wd
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
    wd        <- workdir <$> ask
    externEnv <- (\lenv -> (\(n, v) -> (toS n, toS v)) <$> lenv)
      <$> liftIO SE.getEnvironment
    asyncs <- liftIO $ mkAsyncs l externEnv wd
    _      <- liftIO $ kbInstallHandler $ for_ asyncs cancel
    putV "processes started."
    liftIO (waitAnyCancel asyncs) >>= \(_, x) -> case x of
      Timeout c ->
        tell $ "The following command timed out:" : T.lines (toS $ encodeCmd c)
      DiedFailure c n ->
        tell
          $  "The following command died with exit code "
          <> show n
          <> " :"
          :  T.lines (toS $ encodeCmd c)
      DiedLegal -> putText "All commands exited successfully."
      FoundAll c ->
        putText
          $ "All searched patterns the following command were found. Killing all processes."
          <> mconcat (intersperse "\n" (T.lines (toS $ encodeCmd c)))
      FoundIllegal c t e ->
        tell
          $  "The illegal pattern "
          <> t
          <> " was found in the output of this process' "
          <> stdToS e
          <> ":"
          :  T.lines (toS $ encodeCmd c)
      OutputLacking c e ->
        tell
          $  "This process' "
          <> stdToS e
          <> " was found to be lacking pattern(s):"
          :  T.lines (toS $ encodeCmd c)
      ConduitException c e ->
        tell
          $  "This process' "
          <> stdToS e
          <> " ended with a conduit exception:"
          :  T.lines (toS $ encodeCmd c)
    putV "async step: done"
 where
  mkAsyncs :: [Cmd] -> [(Text, Text)] -> WorkDir -> IO [Async CmdResult]
  mkAsyncs l externEnv wd = for l (async . runCmd externEnv wd)

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

data ProcessWas = Died ExitCode | Killed
runCmd :: [(Text, Text)] -> WorkDir -> Cmd -> IO CmdResult
runCmd fullExternEnv (WorkDir wd) c@Cmd {..} =
  fromMaybe (Timeout c) <$> maybeTimeout
    timeout
    ( withConduitSinks (getfn out) (getfn err)
    $ \outSink errSink -> (PT.withProcess pc $ go outSink errSink)
    )
 where
  go
    :: ConduitT ByteString Void IO ()
    -> ConduitT ByteString Void IO ()
    -> PT.Process
         ()
         (ConduitT () ByteString IO ())
         (ConduitT () ByteString IO ())
    -> IO CmdResult
  go outSink errSink p = do
    conduitOutput <- withAsyncConduitsOnProcess
      p
      ConduitSpec {conduit = outSink, ccheck = filecheck out}
      ConduitSpec {conduit = errSink, ccheck = filecheck err}
      waitEitherCatchCancel
    processOutput <- PT.getExitCode p >>= \case
      Just ec -> return $ Died ec
      Nothing -> PT.stopProcess p >> return Killed
    return $ case (processOutput, conduitOutput) of
      (Died (ExitFailure n), _                    ) -> DiedFailure c n
      (_                   , Left (Right Clean)   ) -> DiedLegal
      (_                   , Right (Right Clean)  ) -> DiedLegal
      (_                   , Left (Right Lacking) ) -> OutputLacking c Out
      (_                   , Right (Right Lacking)) -> OutputLacking c Err
      (_                   , Left (Left e)        ) -> case fromException e of
        Just (ThrowFoundAnAvoid t) -> FoundIllegal c t Out
        Just ThrowFoundAllWants    -> FoundAll c
        Nothing                    -> ConduitException c Out
      (_, Right (Left e)) -> case fromException e of
        Just (ThrowFoundAnAvoid t) -> FoundIllegal c t Err
        Just ThrowFoundAllWants    -> FoundAll c
        Nothing                    -> ConduitException c Err

  pc
    :: PT.ProcessConfig
         ()
         (ConduitT () ByteString IO ())
         (ConduitT () ByteString IO ())
  pc =
    PT.setStdout PT.createSource
      $ PT.setStderr PT.createSource
      $ PT.setStdin PT.closed
      $ PT.setEnv (mapTuple toS <$> envVars vars passvars fullExternEnv)
      $ PT.setWorkingDir (toS wd)
      $ PT.proc (toS name) (toS <$> args)
  getfn :: FileCheck Check -> Text
  getfn fc = wd <> "/" <> toS (filename fc)

maybeTimeout :: Maybe Int -> IO a -> IO (Maybe a)
maybeTimeout i io = case i of
  Nothing -> Just <$> io
  Just t  -> ST.timeout (100000 * t) io

kbInstallHandler :: (MonadIO m) => IO () -> m SPS.Handler
kbInstallHandler h =
  liftIO $ SPS.installHandler SPS.keyboardSignal (SPS.Catch h) Nothing

data ConduitSpec = ConduitSpec {
  conduit :: ConduitT ByteString Void IO (),
  ccheck   :: Check
}

-- | withConduitSinks runs an IO action. It gives two conduit testing asyncs in scope.
-- | THROWS PatternMatched
withAsyncConduitsOnProcess
  :: PT.Process () (ConduitT () ByteString IO ()) (ConduitT () ByteString IO ())
  -> ConduitSpec -- stdout conduit spec
  -> ConduitSpec -- stderr conduit spec
  -> (Async ScanResult -> Async ScanResult -> IO b) -- the lambda-wrapped IO action
  -> IO b
withAsyncConduitsOnProcess p cOut cErr = withAsyncs
  (doFilter (ccheck cOut) (PT.getStdout p) (conduit cOut))
  (doFilter (ccheck cErr) (PT.getStderr p) (conduit cErr))

withAsyncs
  :: IO ScanResult
  -> IO ScanResult
  -> (Async ScanResult -> Async ScanResult -> IO b)
  -> IO b
withAsyncs io1 io2 f =
  liftIO $ withAsync io1 $ \a1 -> withAsync io2 $ \a2 -> f a1 a2

-- | THROWS PatternMatched
doFilter
  :: Check
  -> ConduitT () ByteString IO ()
  -> ConduitT ByteString Void IO ()
  -> IO ScanResult
doFilter behavior source sink =
  runConduit
    $              source
    .|             CB.lines
    .|             makeBehavior behavior
    `fuseUpstream` CC.unlinesAscii
    `fuseUpstream` sink

-- | makeBehavior builds an IO conduit that throws a PatternMatched when
-- all wanted pattern or one avoided pattern are found
makeBehavior :: Check -> ConduitT ByteString ByteString IO ScanResult
makeBehavior Check {..} = case wants of
  [] -> cleanLooper
  as -> expectfulLooper $ toS <$> as
 where

  cleanLooper :: ConduitT ByteString ByteString IO ScanResult
  cleanLooper = noAvoidsAndOtherwise Clean $ \b -> yield b >> cleanLooper

  expectfulLooper
    :: [ByteString] -> ConduitT ByteString ByteString IO ScanResult
  expectfulLooper [] = throw ThrowFoundAllWants
  expectfulLooper l =
    noAvoidsAndOtherwise Lacking
      $ \b -> yield b
          >> expectfulLooper (filter (not . flip B.isInfixOf b) (toS <$> l))

  noAvoidsAndOtherwise endValue otherwiseConduit = await >>= \case
    Just b -> case filter (`B.isInfixOf` b) (toS <$> avoids) of
      []     -> otherwiseConduit b
      xh : _ -> yield b >> throw (ThrowFoundAnAvoid $ toS xh)
    Nothing -> return endValue

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
    run $ lambdaIO $ CB.sinkHandle h
