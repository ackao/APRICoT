module Handler.Search where

import Import
import DB
import qualified Database.Esqueleto as E

getSearchR :: Text -> Handler Html
getSearchR text = do 
        papersByTitle <- getPapersWithTitle text
        papersByAbstract <- getPapersWithAbstract text
        papersByAuthor <- getPapersWithAuthor text
        defaultLayout $ do
            $(widgetFile "search")
