module Handler.Home where

import Import
import DB

getHomeR :: Handler Html
getHomeR = do
    (_uid, user) <- requireAuthPair
    papers <- getPapers
    authorLists <- mapM getAuthorListForPaper papers
    let authorsAndPapers = zip authorLists papers
    defaultLayout $ do
        setTitle . toHtml $ userUsername user <> "'s User page"
        $(widgetFile "homepage")
