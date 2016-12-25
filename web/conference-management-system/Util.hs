module Util where

import Import
import DB

reviewerOpts :: Handler [(Text, Key User)]
reviewerOpts = do
    reviewerEnts <- getReviewers
    return $ map (\(Entity uid user) -> (userUsername user, uid)) reviewerEnts

paperOpts :: Handler [(Text, Key Paper)]
paperOpts = do
    paperEnts <- getAllPapers
    return $ map (\(Entity pid paper) -> (paperTitle paper, pid)) paperEnts 
