{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model.RobotInfo
    ( RobotInfo (..)
    ) where

import Data.Aeson
import GHC.Generics
import Test.QuickCheck
import Model.Number


data RobotInfo = RobotInfo
    { robotId :: Number
    , posX :: Float
    , posY :: Float
    , direction :: Float
    , currentSpeed :: Float
    , requiredSpeed :: Float
    , acceleration :: Float
    , reloadingTime :: Float
    , health :: Float
    , points :: Float
    , missedTurns :: Number
    } deriving (Show, Eq, Generic)

instance FromJSON RobotInfo
instance ToJSON RobotInfo
instance Arbitrary RobotInfo where
    arbitrary = RobotInfo <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
