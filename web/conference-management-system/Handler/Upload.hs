module Handler.Upload where

import Import
import DB
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
    , conflicts :: [Key User]
    }

postUploadR :: Handler Html
postUploadR = do
    reviewerOpts <- reviewers
    ((result, _), _) <- runFormPost $ uploadForm reviewerOpts
    case result of
        FormSuccess (FileForm fi title authors abstract conflicts) -> do
            fileBytes <- runResourceT $ fileSource fi $$ sinkLbs
            (uid, _) <- requireAuthPair
            paperId <- runDB $ insert $ Paper uid (fileName fi)
                            title (unTextarea abstract)
                                 (S.pack . L.unpack $ fileBytes)
            _ <- runDB $ mapM (\author -> insert_ $ Author author paperId) authors
            _ <- runDB $ mapM (\conflict -> insert_ $ Conflict paperId conflict) conflicts
            setMessage "PDF saved"
            redirect HomeR
        _ -> do
            setMessage "Something went wrong"
            redirect UploadR 

getUploadR :: Handler Html
getUploadR = do
    reviewerOpts <- reviewers
    (formWidget, formEnctype) <- generateFormPost $ uploadForm reviewerOpts
    defaultLayout $ do
        $(widgetFile "upload")

uploadForm :: [(Text, Key User)] -> Form FileForm
uploadForm reviewerOpts = renderBootstrap3 BootstrapBasicForm $ FileForm
    <$> fileAFormReq "Choose a file"
    <*> areq textField "Paper Title" Nothing
    <*> areq authorsField "Authors" Nothing
    <*> areq textareaField "Abstract" Nothing
    <*> areq (checkboxesFieldList reviewerOpts) "Conflicts" Nothing

reviewers :: Handler [(Text, Key User)]
reviewers = do
    reviewerEnts <- getReviewers
    return $ map (\(Entity uid user) -> (userUsername user, uid)) reviewerEnts


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
