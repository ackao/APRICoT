module Handler.Home where

import Import

getHomeR :: Handler Html
getHomeR = do
    (_uid, user) <- requireAuthPair
    papers <- getPapers
    authorLists <- mapM getAuthorList papers
    let authorsAndPapers = zip authorLists papers
    defaultLayout $ do
        setTitle . toHtml $ userUsername user <> "'s User page"
        $(widgetFile "homepage")

-- | Gets papers for the currently logged in user
getPapers :: Handler [Entity Paper]
getPapers = do
    (uid, _user) <- requireAuthPair
    papers <- runDB $ selectList [PaperOwner ==. uid] []
    return papers

-- | Given some Entity Paper, returns the authors for the paper.
getAuthorList :: Entity Paper -> Handler [Entity Author]
getAuthorList (Entity paperId _paper) = runDB $ selectList [AuthorPaper ==. paperId] []
