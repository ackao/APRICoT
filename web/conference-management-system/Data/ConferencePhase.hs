{-# LANGUAGE TemplateHaskell #-}
module Data.ConferencePhase where

import Database.Persist.TH
import Prelude

data ConferencePhase = Submission | Review | Decision 
    deriving (Show, Read, Eq)
derivePersistField "ConferencePhase"

