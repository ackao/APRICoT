module Handler.Search where

import Import
import DB
import Data.ConferencePhase
import qualified Database.Esqueleto as E

getSearchR :: Text -> Handler Html
getSearchR text = do 
    Entity _phaseid phase <- getCurrentPhase
    let currentPhase = currentPhasePhase phase 
    case currentPhase of
        Decision -> getDecisionPhaseSearch text
        _ -> defaultLayout $ do
                $(widgetFile "search-default")
        

getDecisionPhaseSearch :: Text -> Handler Html
getDecisionPhaseSearch text = do
        papersByTitle <- getPapersWithTitle text
        papersByAbstract <- getPapersWithAbstract text
        papersByAuthor <- getPapersWithAuthor text
        defaultLayout $ do
            $(widgetFile "search")
