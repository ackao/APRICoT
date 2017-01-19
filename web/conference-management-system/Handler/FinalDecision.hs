module Handler.FinalDecision where

import Import

postFinalDecisionR :: PaperId -> Bool -> Handler Html
postFinalDecisionR paperId accept = do
    runDB $ update paperId [PaperPcAccepted =. accept]
    setMessage "Set final decision"
    redirect ProgramChairR
