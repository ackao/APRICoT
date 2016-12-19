module Handler.ReviewPaper where

import Import
import Data.PaperStatus

import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

data ReviewForm = Form 
    { status :: PaperStatus 
    , comment :: Textarea
    }

getReviewPaperR :: ReviewId -> Handler Html
getReviewPaperR reviewId = do
    (formWidget, formEnctype) <- generateFormPost reviewForm
    defaultLayout $ do
        $(widgetFile "review-paper")

postReviewPaperR :: ReviewId -> Handler Html
postReviewPaperR reviewId = do
    ((result, _), _) <- runFormPost reviewForm
    case result of
        FormSuccess (Form status comment) -> do
            runDB $ update reviewId [ReviewStatus =. status
                                    , ReviewComments =. (unTextarea comment)]
            setMessage "Review Saved"
            redirect ReviewR
        _ -> do
            setMessage "Something went wrong"
            redirect $ ReviewPaperR reviewId 

reviewForm :: Form ReviewForm
reviewForm = renderBootstrap3 BootstrapBasicForm $ Form 
    <$> areq (selectFieldList reviews) "Review" Nothing 
    <*> areq textareaField "Comments" Nothing
  where
    reviews :: [(Text, PaperStatus)]
    reviews = [("Strong Reject", SReject), ("Weak Reject", WReject), ("Neutral", Neutral),
               ("Weak Accept", WAccept), ("Strong Accept", SAccept)]
