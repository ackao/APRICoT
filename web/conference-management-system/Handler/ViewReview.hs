module Handler.ViewReview where

import Import
import DB

-- | isAuthorized ensures that an author can only see this page if the
-- conference is in the decision phase.
-- This might end up being an interesting function? How will we take
-- information from isAuthorized to infer that the data disclosed here is
-- atually safe to disclose?
getViewReviewR :: PaperId -> Handler Html
getViewReviewR paperId = do
    reviews <- getReviewsForPaper paperId
    paper <- runDB $ get404 paperId
    defaultLayout $ do
        $(widgetFile "view-review")
