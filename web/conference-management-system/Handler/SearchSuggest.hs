module Handler.SearchSuggest where

import Import
import DB

getSearchSuggestR :: Handler Value
getSearchSuggestR = do
        query <- fromMaybe "" <$> lookupGetParam "term"
        users <- getUsersWithName query
        returnJson ((map (\(Entity _userId user) -> userEmailAddress user) users) :: [Text])
