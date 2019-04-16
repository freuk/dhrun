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

import qualified Dhall
import           Dhrun.Types
import           Dhrun.AesonTypes               ( encodeCmd )
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
                                                ( isInfixOf
                                                , lines
                                                )
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
                                                , throw
                                                )
import qualified System.Directory              as SD
                                                ( createDirectoryIfMissing )
import qualified System.Environment            as SE
                                                ( getEnvironment )
import qualified Data.Map.Lazy                 as DM
                                                ( fromList
                                                , toList
                                                )
import qualified Data.Map.Merge.Lazy           as DMM
                                                ( merge
                                                , preserveMissing
                                                , zipWithMatched
                                                )
import           Control.Arrow                  ( (***) )
import qualified System.Timeout                as ST

data CmdResult =
      Timeout Cmd
    | DiedLegal
    | DiedFailure Cmd Int
    | FoundAll Cmd
    | FoundIllegal Cmd Std
    | OutputLacking Cmd Std
    | ConduitException Cmd Std deriving (Show)

data MonitoringResult =
    ThrowFoundAllWants
  | ThrowFoundAnAvoid
  deriving (Show, Typeable)

instance Exception MonitoringResult

data ScanResult =  Clean | Lacking  deriving (Show)

data Std = Out | Err deriving (Show)
stdToS :: Std -> Text
stdToS Out = "stdout"
stdToS Err = "stderr"

btw :: (Functor f) => (t -> f b) -> t -> f t
btw k x = x <$ k x

btwc :: (Functor f) => f b -> b1 -> f b1
btwc m = btw (const m)

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
 where
  runPre  = runMultipleV pre "pre-processing"
  runPost = runMultipleV post "post-processing"

putV :: (MonadIO m, MonadReader DhallExec m) => Text -> m ()
putV text =
  (== Verbose) . verbosity <$> ask >>= flip when (liftIO $ putText text)

runChecks :: (MonadIO m, MonadReader DhallExec m, MonadWriter [Text] m) => m ()
runChecks = (cmds <$> ask) >>= \case
  [] -> putV "post-mortem check step: no processes."
  l  -> do
    putV "post-mortem checks: start"
    for_ l $ \Cmd {..} -> do
      putV $ "running post-mortem checks for " <> name <> " " <> mconcat
        (intersperse " " args)
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

runAsyncs :: (MonadIO m, MonadReader DhallExec m, MonadWriter [Text] m) => m ()
runAsyncs = (cmds <$> ask) >>= \case
  [] -> putV "async step: no processes."
  l  -> do
    putV "async step: start"
    wd <- toS . workdir <$> ask >>= btw
      (liftIO . SD.createDirectoryIfMissing False)
    externEnv <- (\lenv -> (\(n, v) -> (toS n, toS v)) <$> lenv)
      <$> liftIO SE.getEnvironment
    asyncs <- liftIO $ mkAsyncs l externEnv $ toS wd
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
      FoundIllegal c e ->
        tell
          $  "An illegal pattern was found in the output of this process' "
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
  mkAsyncs :: [Cmd] -> [(Text, Text)] -> Text -> IO [Async CmdResult]
  mkAsyncs l externEnv wd = for l (async . runCmd externEnv wd)


runMultipleV
  :: (MonadIO m, MonadReader DhallExec m)
  => (DhallExec -> [Text])
  -> Text
  -> m ()
runMultipleV getter desc = getter <$> ask >>= \case
  [] -> putV $ "no " <> desc <> " commands to run."
  l  -> do
    putV $ desc <> ": start"
    wd <- toS . workdir <$> ask >>= btw
      (liftIO . SD.createDirectoryIfMissing False)
    for_ l
      $ \x -> liftIO (runProcess $ setWorkingDir wd (shell $ toS x)) >>= \case
          ExitSuccess -> putV ("ran one " <> desc <> " command.")
          ExitFailure _ ->
            liftIO $ die $ "failed to execute one " <> desc <> " command." <> x
    putV $ desc <> ": done"

runCmd :: [(Text, Text)] -> Text -> Cmd -> IO CmdResult
runCmd fullExternEnv wd c@Cmd {..} = goThrowsTimeout
 where
  goThrowsUsingAsyncs
    :: ConduitT ByteString Void IO ()
    -> ConduitT ByteString Void IO ()
    -> Process
         ()
         (ConduitT () ByteString IO ())
         (ConduitT () ByteString IO ())
    -> IO CmdResult
  goThrowsUsingAsyncs outSink errSink p =
    withAsyncConduitsOnProcess
        p
        ConduitSpec {conduit = outSink, ccheck = filecheck out}
        ConduitSpec {conduit = errSink, ccheck = filecheck err}
        waitEitherCatch
      >>= btwc (stopProcess p)
      >>= return
      <$> \case
            Left  (Right Clean  ) -> DiedLegal
            Right (Right Clean  ) -> DiedLegal
            Left  (Right Lacking) -> OutputLacking c Out
            Right (Right Lacking) -> OutputLacking c Err
            Left  (Left  e      ) -> case fromException e of
              Just ThrowFoundAnAvoid  -> FoundIllegal c Out
              Just ThrowFoundAllWants -> FoundAll c
              Nothing                 -> ConduitException c Out
            Right (Left e) -> case fromException e of
              Just ThrowFoundAnAvoid  -> FoundIllegal c Err
              Just ThrowFoundAllWants -> FoundAll c
              Nothing                 -> ConduitException c Err

  goThrowsTimeoutUsingTheseSinks
    :: ConduitT ByteString Void IO ()
    -> ConduitT ByteString Void IO ()
    -> IO CmdResult
  goThrowsTimeoutUsingTheseSinks outSink errSink =
    fromMaybe (Timeout c) <$> maybeTimeout
      timeout
      (withProcess pc $ goThrowsUsingAsyncs outSink errSink)

  goThrowsTimeout :: IO CmdResult
  goThrowsTimeout =
    withConduitSinks (getfn out) (getfn err) goThrowsTimeoutUsingTheseSinks

  forcedEnvVars :: [(Text, Text)]
  forcedEnvVars = (\EnvVar {..} -> (toS varname, toS value)) <$> vars
  externEnvVars :: [(Text, Text)]
  externEnvVars = filter (\(k, _) -> k `elem` passvars) fullExternEnv
  envVars :: [(Text, Text)]
  envVars = DM.toList $ DMM.merge DMM.preserveMissing
                                  DMM.preserveMissing
                                  (DMM.zipWithMatched (\_ x _ -> x))
                                  (DM.fromList forcedEnvVars)
                                  (DM.fromList externEnvVars)

  pc
    :: ProcessConfig
         ()
         (ConduitM () ByteString IO ())
         (ConduitM () ByteString IO ())
  pc =
    configureConduits
      $ setEnv (mapTuple toS <$> envVars)
      $ setWorkingDir (toS wd)
      $ proc (toS name) (toS <$> args)
  getfn :: FileCheck Check -> Text
  getfn fc = wd <> "/" <> filename fc

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple = join (***)

maybeTimeout :: Maybe Dhall.Natural -> IO a -> IO (Maybe a)
maybeTimeout i io = case i of
  Nothing -> Just <$> io
  Just t  -> ST.timeout (fromInteger $ toInteger t) io

kbInstallHandler :: (MonadIO m) => IO () -> m Handler
kbInstallHandler h = liftIO $ installHandler keyboardSignal (Catch h) Nothing

data ConduitSpec = ConduitSpec {
  conduit :: ConduitT ByteString Void IO (),
  ccheck   :: Check
}

-- | withConduitSinks runs an IO action. It gives two conduit testing asyncs in scope.
-- | THROWS PatternMatched
withAsyncConduitsOnProcess
  :: Process () (ConduitT () ByteString IO ()) (ConduitT () ByteString IO ())
  -> ConduitSpec --stdout conduit spec
  -> ConduitSpec --stderr conduit spec
  -> (Async ScanResult -> Async ScanResult -> IO b) -- the lambda-wrapped IO action
  -> IO b
withAsyncConduitsOnProcess p cOut cErr = withAsyncs
  (doFilter (ccheck cOut) (getStdout p) (conduit cOut))
  (doFilter (ccheck cErr) (getStderr p) (conduit cErr))
 where
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

-- | makeBehavior builds an IO conduit that THROWS A PatternMatched.
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
  expectfulLooper l  = noAvoidsAndOtherwise Lacking $ \b -> yield b
    >> expectfulLooper (filter (\x -> not $ B.isInfixOf x b) (toS <$> l))

  noAvoidsAndOtherwise endValue otherwiseConduit = await >>= \case
    Just b | any (`B.isInfixOf` b) (toS <$> avoids) -> throw ThrowFoundAnAvoid
           | otherwise                              -> otherwiseConduit b
    Nothing -> return endValue

configureConduits
  :: ProcessConfig () () ()
  -> ProcessConfig
       ()
       (ConduitM () ByteString IO ())
       (ConduitM () ByteString IO ())
configureConduits p =
  setStdout createSource $ setStderr createSource $ setStdin closed p

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
