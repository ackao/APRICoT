module Handler.Review where

import Import
import DB
import qualified Database.Esqueleto as E

getReviewR :: Handler Html
getReviewR = do
    papers <- getPapersToReview
    defaultLayout $ do
        $(widgetFile "review")
