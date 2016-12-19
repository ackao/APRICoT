module Handler.Upload where

import Import
import Text.Julius
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Conduit.Binary

import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

data FileForm = FileForm
    { fileInfo :: FileInfo
    , title    :: Text
    , authors  :: [Text]
    , abstract :: Textarea
    }

postUploadR :: Handler Html
postUploadR = do
    ((result, _), _) <- runFormPost uploadForm
    case result of
        FormSuccess (FileForm fi title authors abstract) -> do
            fileBytes <- runResourceT $ fileSource fi $$ sinkLbs
            (uid, _) <- requireAuthPair
            paperId <- runDB $ insert $ Paper uid (fileName fi)
                            title (unTextarea abstract)
                                 (S.pack . L.unpack $ fileBytes)
            _ <- runDB $ mapM (\author -> insert_ $ Author author paperId) authors
            setMessage "PDF saved"
            redirect HomeR
        _ -> do
            setMessage "Something went wrong"
            redirect UploadR 

getUploadR :: Handler Html
getUploadR = do
    (formWidget, formEnctype) <- generateFormPost uploadForm
    defaultLayout $ do
        $(widgetFile "upload")

uploadForm :: Form FileForm
uploadForm = renderBootstrap3 BootstrapBasicForm $ FileForm
    <$> fileAFormReq "Choose a file"
    <*> areq textField "Paper Title" Nothing
    <*> areq authorsField "Authors" Nothing
    <*> areq textareaField "Abstract" Nothing

authorsField :: Field Handler [Text]
authorsField = Field
    { fieldParse = \rawVals _fileVals ->
        case rawVals of
            (x:xs) -> return $ Right $ Just (x:xs)
            [] -> return $ Left "You must add at least one author" 
    , fieldView = \idAttr nameAttr _otherAttrs _eResult _isReq ->
        let _ =  $(juliusFileReload "templates/authors-form.julius") in
        $(widgetFile "authors-form")
    , fieldEnctype = UrlEncoded
    }
