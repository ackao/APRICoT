module Handler.Search where

import Import
import qualified Data.Text as T
import qualified Database.Esqueleto as E
import Database.Esqueleto ((^.))

getSearchR :: Text -> Handler Html
getSearchR text = do 
        papersByTitle <- getPapersWithTitle text
        papersByAbstract <- getPapersWithAbstract text
        papersByAuthor <- getPapersWithAuthor text
        defaultLayout $ do
            $(widgetFile "search")

-- | A filter for "LIKE" queries in SQL. Unfortunately, this does not come for
-- free in Yesod.Persistent as it is backend specific. As a result, we have to
-- use ugly string literals.
like :: EntityField a Text -> Text -> Filter a 
like field val = Filter field (Left $ T.concat ["%", val, "%"])
                              (BackendSpecificFilter "like")

getPapersWithTitle :: Text -> Handler [Entity Paper]
getPapersWithTitle title = runDB $ selectList [like PaperTitle title] []

getPapersWithAbstract :: Text -> Handler [Entity Paper]
getPapersWithAbstract abstract = runDB $ selectList [like PaperAbstract abstract] []

getPapersWithAuthor :: (YesodPersist site, YesodPersistBackend site ~ SqlBackend) =>
        Text -> HandlerT site IO [(E.Value (Key Paper), E.Value Text, E.Value Text, E.Value Text)]
getPapersWithAuthor authorName = do
    papers <- runDB
           $ E.select
           $ E.from $ \(author `E.InnerJoin` paper) -> do
                E.on $ (paper ^. PaperId E.==. author ^. AuthorPaper) E.&&.
                  ((author ^. AuthorAuthor) `E.like` (E.%) E.++. E.val authorName E.++. (E.%))
                return
                    ( paper ^. PaperId
                    , paper ^. PaperTitle
                    , paper ^. PaperFilepath
                    , paper ^. PaperAbstract 
                    )
    return papers
