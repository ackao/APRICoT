module Handler.Review where

import Import
import qualified Database.Esqueleto as E
import Database.Esqueleto ((^.))

import Data.PaperStatus

getReviewR :: Handler Html
getReviewR = do
    papers <- getPapersToReview
    defaultLayout $ do
        $(widgetFile "review")

getPapersToReview :: Handler [(E.Value (Key Review)
                             , E.Value PaperStatus
                             , E.Value Text
                             , E.Value Text
                             , E.Value Text)]
getPapersToReview = do
    (uid, _user) <- requireAuthPair
    papers <- runDB
           $ E.select
           $ E.from $ \(review `E.InnerJoin` paper) -> do
                E.on $ (paper ^. PaperId E.==. review ^. ReviewPaper ) E.&&.
                  ((review ^. ReviewUser) E.==. E.val uid)
                return
                    ( review ^. ReviewId 
                    , review ^. ReviewStatus
                    , review ^. ReviewComments
                    , paper ^. PaperTitle
                    , paper ^. PaperAbstract 
                    )
    return papers
