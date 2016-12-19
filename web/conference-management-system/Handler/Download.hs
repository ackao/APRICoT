{-# LANGUAGE OverloadedStrings #-}

-- | This module contains the GET handler for hyperlinks to files.
module Handler.Download where

import Import
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text


-- | Search the application\'s 'Store' for a file keyed on ident. Return a 404
-- HTTP response if no file is found. In the case that the file exists, return
-- it along with appropriate HTTP headers so that web browsers will download
-- it as a file.
getDownloadR :: Key Paper -> Handler TypedContent
getDownloadR ident = do
    -- Attempt to retrieve the file, failing with a 404.
    Paper _owner filename _title _abstract bytes <- getById ident
    -- The Content-Disposition header hints that the resource should be
    -- downloaded as a file.
    addHeader "Content-Disposition" $ Text.concat
        [ "attachment; filename=\"", filename, "\""]
    sendResponse (Text.encodeUtf8 $ "application/pdf", toContent bytes)

-- | Attempt to get a paper given a Key. If no paper can be found, we return
-- a 404 response.
getById :: Key Paper -> Handler Paper 
getById ident = do
    mfile <- runDB $ get ident
    case mfile of
      Nothing -> notFound
      Just file -> return file
