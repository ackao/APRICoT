module Handler.AssignPaper where

import Import
import qualified Handler.ProgramChair as PC
import qualified Util as U
import Data.PaperStatus

postAssignPaperR :: Handler Html
postAssignPaperR = do
    reviewerOpts <- U.reviewerOpts
    paperOpts <- U.paperOpts
    ((assignResult, _), _) <- runFormPost $ PC.assignPaperForm reviewerOpts paperOpts
    case assignResult of
        FormSuccess (PC.AssignPaperForm userId paperId) -> do
            runDB $ insert_ $ Review paperId userId NotReviewed ""
            setMessage "Paper Assigned"
            redirect ProgramChairR
        _ -> do
            setMessage "Something went wrong"
            redirect $ ProgramChairR
