module Handler.ProgramChair where

import Import
import DB
import qualified Database.Esqueleto as E
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

addReviewerForm :: Form AddReviewerForm
addReviewerForm = renderBootstrap3 BootstrapBasicForm $ Form 
    <$> areq textField "Add Reviewer" Nothing 
