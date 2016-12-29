module Handler.Ready where

import Import

postReadyR :: PaperId -> Handler Html
postReadyR paperId = do
    currPaper <- runDB $ get404 paperId
    let changedSetting = not $ paperReady currPaper
    runDB $ update paperId [PaperReady =. changedSetting]
    setMessage "Changed Paper Status"
    redirect HomeR
