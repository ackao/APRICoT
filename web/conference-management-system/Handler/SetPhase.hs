module Handler.SetPhase where

import Import
import qualified Handler.ProgramChair as PC

postSetPhaseR :: Handler Html
postSetPhaseR = do
    ((phaseResult, _), _) <- runFormPost $ PC.setPhaseForm
    case phaseResult of
        FormSuccess (PC.SetPhaseForm phase) -> do
            Entity currentPhaseId _currentPhase <- getCurrentPhase
            runDB $ update currentPhaseId [CurrentPhasePhase =. phase]
            setMessage "Phase changed successfully"
            redirect ProgramChairR
        _ -> do
            setMessage "Something went wrong"
            redirect $ ProgramChairR
