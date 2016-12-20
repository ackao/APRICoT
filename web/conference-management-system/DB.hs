module DB where

import Import
import Data.PaperStatus
import qualified Database.Esqueleto as E
import Database.Esqueleto ((^.))
import qualified Data.Text as T

-- | Attempt to get a paper given a Key. If no paper can be found, we return
-- a 404 response.
-- Lifty/LH: u:User
--           -> w:World 
--           -> {id:Key Paper | not (inPaperConflicts u w)} 
--           -> Tagged Handler Paper
-- UrFlow: 
-- policy sendClient (SELECT *
--         FROM conflicts, paper, user
--         WHERE conflicts.paper = paper.Id
--           AND conflicts.user <> user.Id
--           AND known(user.pass))
getPaperById :: Key Paper -> Handler Paper 
getPaperById ident = do
    mfile <- runDB $ get ident
    case mfile of
      Nothing -> notFound
      Just file -> return file

-- | Gets papers for the currently logged in user
-- Lifty/LH: u:User (implicit)
--           -> w:World
--           -> Tagged Handler [{paper:Entity Paper | paper ^. owner = user ^. id}]  
--              
-- UrFlow:
-- policy sendClient (SELECT *
--         FROM paper, user
--         WHERE known(user.pass)
--           AND paper.owner = user.Id)
getPapers :: Handler [Entity Paper]
getPapers = do
    (uid, _user) <- requireAuthPair
    papers <- runDB $ selectList [PaperOwner ==. uid] []
    return papers

-- | Given some Entity Paper, returns the authors for the paper.
-- Lifty/LH: u:User
--           -> {w:World | not (inPaperConflicts u w)}
--           -> {p: Tagged Entity Paper | p ^. owner = user ^. id} 
--           -> Tagged Handler [{a:Entity Author | p ^. id = a ^. id}]
-- UrFlow:
-- policy sendClient (SELECT *
--         FROM paper, user, author
--         WHERE known(user.pass)
--           AND author.paper = paper.id
getAuthorListForPaper :: Entity Paper -> Handler [Entity Author]
getAuthorListForPaper  (Entity paperId _paper) =
    runDB $ selectList [AuthorPaper ==. paperId] []

-- | Gets all the reviewers in the system
-- Note: This is not sensitive data
-- Lifty/LH: u:User
--           -> w:World
--           -> Handler [{user:Entity User | isReviewer user w}]
--
-- UrFlow:
-- policy sendClient (SELECT *
--         FROM reviewer
--         WHERE known(user.pass))
getReviewers :: Handler [Entity User]
getReviewers = runDB $ selectList [UserReviewer ==. True] []

-- | Gets all the papers for a reviewer
-- Only the PC should be able to see this
-- Paper should be ready
-- Lifty/LH: u:User
--           -> {w:World | isPC u w}
--           -> Entity User
--           -> Tagged Handler [(E.Value Text, E.Value (Key Paper)]
--
-- UrFlow:
-- policy sendClient(SELECT *
--          FROM user
--          WHERE known(user.pass)
--            AND user.isPc = 1)
getPapersForReviewer :: Entity User -> Handler [(E.Value Text, E.Value (Key Paper))]
getPapersForReviewer (Entity uid _user) = do
    papers <- runDB
           $ E.select
           $ E.from $ \(review `E.InnerJoin` paper) -> do
                E.on $ (paper ^. PaperId E.==. review ^. ReviewPaper ) E.&&.
                  ((review ^. ReviewUser) E.==. E.val uid)
                return
                    ( paper ^. PaperTitle
                    , paper ^. PaperId
                    )
    return papers

-- | Gets the Entity User given a username
-- Note: This is not sensitive data
-- Lifty/LH: User
--           -> World
--           -> Text
--           -> Handler (Entity User)
-- UrFlow:
-- policy sendClient (SELECT *
--         FROM user
--         WHERE (known user.pass)
getUserForUsername :: Text -> Handler (Entity User)
getUserForUsername userName = do 
    users <- runDB $ selectList [UserUsername ==. userName] []
    case users of
        [] -> error ("User does not exist: " ++ (T.unpack userName))
        [x] -> return x
        _ -> error ("Username was not unique: " ++ (T.unpack userName))

-- | Gets the papers the currently logged in user was assigned to review.
-- Lifty/LH: u:User
--           -> w:World
--           -> Tagged Handler [{(review:E.Value (Key Review), ... |
--                  review ^. user = u ^. id }]
--
-- UrFlow:
-- policy sendClient (SELECT *
--         FROM user, review
--         WHERE known(user.pass)
--           AND user.id = review.user
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

-- | SEARCH METHODS

-- | A filter for "LIKE" queries in SQL. Unfortunately, this does not come for
-- free in Yesod.Persistent as it is backend specific. As a result, we have to
-- use ugly string literals.
like :: EntityField a Text -> Text -> Filter a 
like field val = Filter field (Left $ T.concat ["%", val, "%"])
                              (BackendSpecificFilter "like")

-- | Gets papers whose titles are a super string of the input string
-- Lift/LH: u:User
--          -> {w:World | inDecisionPhase w}
--          -> s:Text
--          -> Handler [{p:Entity Paper | p ^. accepted}]
--          
-- UrFlow:
-- policy sendClient (SELECT *
--         FROM phase, paper, review
--         WHERE phase = 3
--           AND review.id = paper.id
--           AND review.status = Accepted
getPapersWithTitle :: Text -> Handler [Entity Paper]
getPapersWithTitle title = runDB $ selectList [like PaperTitle title] []

-- | Gets papers with an abstract containing the input string
-- NOTE: policy is the same for all search functions
-- Lift/LH: u:User
--          -> {w:World | inDecisionPhase w}
--          -> s:Text
--          -> Handler [{p:Entity Paper | p ^. accepted}]
--          
-- UrFlow:
-- policy sendClient (SELECT *
--         FROM phase, paper, review
--         WHERE phase = 3
--           AND review.id = paper.id
--           AND review.status = Accepted
getPapersWithAbstract :: Text -> Handler [Entity Paper]
getPapersWithAbstract abstract = runDB $ selectList [like PaperAbstract abstract] []

-- | Gets papers with authors matching the input string 
-- Lift/LH: u:User
--          -> {w:World | inDecisionPhase w}
--          -> s:Text
--          -> Handler [{p:Entity Paper | p ^. accepted}]
--          
-- UrFlow:
-- policy sendClient (SELECT *
--         FROM phase, paper, review
--         WHERE phase = 3
--           AND review.id = paper.id
--           AND review.status = Accepted
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
