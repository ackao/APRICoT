module Handler.Profile where

import Import

getProfileR :: Handler Html
getProfileR = do
    (_uid, user) <- requireAuthPair
    defaultLayout $ do
        setTitle . toHtml $ userUsername user <> "'s User page"
        $(widgetFile "profile")

