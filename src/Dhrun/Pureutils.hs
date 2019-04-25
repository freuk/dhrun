{-# language DerivingStrategies #-}
{-# language FlexibleContexts #-}
{-# language RecordWildCards #-}
{-# language DataKinds #-}
{-# language FlexibleInstances #-}
{-# language ScopedTypeVariables #-}
{-# language TypeOperators #-}
{-# language NoImplicitPrelude #-}

{-|
Module      : Dhrun.Run
Description : runner
Copyright   : (c) Valentin Reis, 2018
License     : MIT
Maintainer  : fre@freux.fr
-}

module Dhrun.Pureutils
  ( envVars
  , mapTuple
  )
where

import           Dhrun.Cfg
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
import           Control.Arrow                  ( (***) )

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
