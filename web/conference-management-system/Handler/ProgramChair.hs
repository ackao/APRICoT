module Handler.ProgramChair where

import Import
import qualified Database.Esqueleto as E
import Database.Esqueleto ((^.))
import qualified Data.Text as T

import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

data AddReviewerForm = Form 
    { userName :: Text
    }

getProgramChairR :: Handler Html
getProgramChairR = do 
    reviewers <- getReviewers
    papersForReviewers <- mapM getPapersForReviewer reviewers
    let reviewersAndPapers = zip reviewers papersForReviewers
    (formWidget, formEnctype) <- generateFormPost addReviewerForm
    defaultLayout $ do
        $(widgetFile "pc")

postProgramChairR :: Handler Html
postProgramChairR = do
    ((result, _), _) <- runFormPost addReviewerForm 
    case result of
        FormSuccess (Form userName) -> do
            Entity uid _user <- getUserForUsername userName
            runDB $ update uid [UserReviewer =. True]
            setMessage "Review Saved"
            redirect ProgramChairR
        _ -> do
            setMessage "Something went wrong"
            redirect $ ProgramChairR

-- | Gets all the reviewers in the system
getReviewers :: Handler [Entity User]
getReviewers = runDB $ selectList [UserReviewer ==. True] []

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

getUserForUsername :: Text -> Handler (Entity User)
getUserForUsername userName = do 
    users <- runDB $ selectList [UserUsername ==. userName] []
    case users of
        [] -> error ("User does not exist: " ++ (T.unpack userName))
        [x] -> return x
        _ -> error ("Username was not unique: " ++ (T.unpack userName))

addReviewerForm :: Form AddReviewerForm
addReviewerForm = renderBootstrap3 BootstrapBasicForm $ Form 
    <$> areq textField "Add Reviewer" Nothing 
