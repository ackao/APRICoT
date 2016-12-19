{-# LANGUAGE TemplateHaskell #-}
module Data.PaperStatus where

import Database.Persist.TH
import Prelude

data PaperStatus = SReject | WReject | Neutral | WAccept | SAccept | NotReviewed
    deriving (Show, Read, Eq)
derivePersistField "PaperStatus"
