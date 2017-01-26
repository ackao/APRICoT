module Foundation where

import Import.NoFoundation
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet          (hamletFile)
import Text.Jasmine         (minifym)

-- Used only when in "auth-dummy-login" setting is enabled.
import Yesod.Auth.Dummy

import Yesod.Default.Util   (addStaticContentExternal)
import Yesod.Core.Types     (Logger)
import Yesod.Auth.Account
import qualified Yesod.Core.Unsafe as Unsafe
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Text.Email.Validate as E
import Network.Mail.Mime
import Network.Mail.Client.Gmail
import System.Environment
import qualified Auth.Account as Auth
import Yesod.Auth.Message (AuthMessage (InvalidLogin))
import Yesod.Form.Jquery (YesodJquery)
import Data.ConferencePhase

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

data MenuItem = MenuItem
    { menuItemLabel :: Text
    , menuItemRoute :: Route App
    , menuItemAccessCallback :: Bool
    }

data MenuTypes
    = NavbarLeft MenuItem
    | NavbarRight MenuItem


-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware = defaultYesodMiddleware

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage

        muser <- maybeAuthPair
        mcurrentRoute <- getCurrentRoute

        ((formRes, searchWidget), _) <- runFormGet searchForm
        _ <- case formRes of
            FormSuccess qstring -> redirect $ SearchR qstring 
            _ -> return ()

        -- Get the breadcrumbs, as defined in the YesodBreadcrumbs instance.
        (title, parents) <- breadcrumbs

        -- Define the menu items of the header.
        let mItems =
                [ NavbarLeft $ MenuItem
                    { menuItemLabel = "Home"
                    , menuItemRoute = HomeR
                    , menuItemAccessCallback = True
                    }
                , NavbarLeft $ MenuItem
                    { menuItemLabel = "Upload"
                    , menuItemRoute = UploadR 
                    , menuItemAccessCallback = isJust muser
                    }
                , NavbarRight $ MenuItem
                    { menuItemLabel = "Login"
                    , menuItemRoute = AuthR LoginR
                    , menuItemAccessCallback = isNothing muser
                    }
                , NavbarRight $ MenuItem
                    { menuItemLabel = "Logout"
                    , menuItemRoute = AuthR LogoutR
                    , menuItemAccessCallback = isJust muser
                    }
                ]

        let mItemsReviewer = case muser of 
                        Nothing -> mItems 
                        Just (_uid, user) -> if (userReviewer user) then mItems ++
                            [NavbarLeft $ MenuItem
                                { menuItemLabel = "Review"
                                , menuItemRoute = ReviewR 
                                , menuItemAccessCallback = isJust muser}]
                            else mItems 

        let menuItems = case muser of 
                            Nothing -> mItemsReviewer
                            Just (_uid, user) -> if (userPc user) then mItemsReviewer ++
                                [NavbarLeft $ MenuItem
                                    { menuItemLabel = "Program Chair"
                                    , menuItemRoute = ProgramChairR
                                    , menuItemAccessCallback = isJust muser}]
                                else mItemsReviewer

        let navbarLeftMenuItems = [x | NavbarLeft x <- menuItems]
        let navbarRightMenuItems = [x | NavbarRight x <- menuItems]

        let navbarLeftFilteredMenuItems = [x | x <- navbarLeftMenuItems, menuItemAccessCallback x]
        let navbarRightFilteredMenuItems = [x | x <- navbarRightMenuItems, menuItemAccessCallback x]

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_bootstrap_css
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authentication.
    -- *POLICIES GO HERE*
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized HomeR _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    isAuthorized (StaticR _) _ = return Authorized

    isAuthorized UploadR _ = isAuthenticated
    isAuthorized (DownloadR p) _ = isDownloadAuthenticated p 
    isAuthorized (SearchR _) _ = isAuthenticated
    isAuthorized ReviewR _ = isReviewerAuthenticated
    isAuthorized (ReviewPaperR p) _ = isReviewPaperAuthenticated p
    isAuthorized ProgramChairR _ = isPcAuthenticated
    isAuthorized AssignPaperR _ = isPcAuthenticated
    isAuthorized (ReadyR p)_ = isReadyAuthenticated p
    isAuthorized SetPhaseR _ = isPcAuthenticated
    isAuthorized (FinalDecisionR _ _) _ = isPcAuthenticated
    isAuthorized SearchSuggestR _ = return Authorized
    isAuthorized (ViewReviewR p) _ = isViewReviewAuthenticated p 

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

-- Search Bar Form
searchForm :: Html -> MForm Handler (FormResult Text, Widget)
searchForm = renderDivs $ areq (searchField False) "Search" Nothing

-- Define breadcrumbs.
instance YesodBreadcrumbs App where
  breadcrumb HomeR = return ("Home", Nothing)
  breadcrumb (AuthR _) = return ("Login", Just HomeR)
  breadcrumb UploadR = return ("Upload", Just HomeR)
  breadcrumb ReviewR  = return ("Review", Just HomeR)
  breadcrumb (ReviewPaperR _) = return ("Review Paper", Just HomeR)
  breadcrumb ProgramChairR = return ("Program Chair", Just HomeR)
  breadcrumb (ViewReviewR _) = return ("See Reviews", Just HomeR)
  breadcrumb (SearchR _) = return ("Search", Just HomeR)
  breadcrumb  _ = return ("home", Nothing)

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer _ = True

    authenticate creds = runDB $ do
        x <- getBy $ UniqueUsername $ credsIdent creds
        return $ case x of
            Just (Entity uid _) -> Authenticated uid
            Nothing -> UserError InvalidLogin

    -- You can add other plugins like Google Email, email or OAuth here
    authPlugins app = [
                      accountPlugin
                      ] ++ extraAuthPlugins
        -- Enable authDummy login if enabled.
        where extraAuthPlugins = [authDummy | appAuthDummyLogin $ appSettings app]

    authHttpManager = getHttpManager

getEmailCredentials :: IO (Text, Text)
getEmailCredentials = do
    email <- getEnv "GOOGLE_USERNAME"
    password <- getEnv "GOOGLE_PASSWORD"
    return (T.pack email, T.pack password)

sendEmail :: Text -> Text -> Text -> IO ()
sendEmail to subject body = do
    (from, password) <- liftIO getEmailCredentials
    sendGmail (fromStrict from)
              (fromStrict password)
              (Address (Just "Sig Bovik") from)
              [Address Nothing to]
              []
              []
              subject
              (fromStrict body)
              []
              10000000


instance AccountSendEmail App
    where
    sendVerifyEmail user email url = do
    liftIO $ sendEmail email
                  "Confirm your account"
                  ("Hi " ++ user ++ ",\n\nPlease click this link \
                   \ to confirm your account:\n" ++ url)
    where
    sendNewPasswordEmail user email url = do
    liftIO $ sendEmail email
                  "Reset your password"
                  ("Hi " ++ user ++ ",\n\nPlease click this link \
                   \ to reset your password:\n" ++ url)

instance YesodJquery App

instance YesodAuthAccount (AccountPersistDB App User) App where
    runAccountDB = runAccountPersistDB
    getNewAccountR = Auth.getNewAccountR
    postNewAccountR = Auth.postNewAccountR
    checkValidUsername u | E.isValid (TE.encodeUtf8 u) = return $ Right u
    checkValidUsername _ = do
        mr <- getMessageRender
        return $ Left $ mr InvalidLogin 

-- | Access function to determine if a user is logged in.
isAuthenticated :: Handler AuthResult
isAuthenticated = do
    muid <- maybeAuthId
    return $ case muid of
        Nothing -> Unauthorized "You must login to access this page"
        Just _ -> Authorized

-- | Access function to determine if a user is a reviewer
isReviewerAuthenticated :: Handler AuthResult
isReviewerAuthenticated = do
    pair <- maybeAuthPair
    let msg = "You must be a reviewer to access this page"
    return $ case pair of
        Nothing -> Unauthorized msg
        Just (_id, user) -> if (userReviewer user) then Authorized else Unauthorized msg

-- | Access function to determine if a user is a Pc 
isPcAuthenticated :: Handler AuthResult
isPcAuthenticated = do
    pair <- maybeAuthPair
    let msg = "You must be the program chair to access this page"
    return $ case pair of
        Nothing -> Unauthorized msg
        Just (_id, user) -> if (userPc user) then Authorized else Unauthorized msg

-- | Access function to determine if a user is authenticated for the paper
isReadyAuthenticated :: PaperId -> Handler AuthResult
isReadyAuthenticated p = do
    pair <- maybeAuthPair
    let msg = "You do not have access to this page"
    case pair of
        Nothing -> return $ Unauthorized msg
        Just (uid, _user) -> do 
            isAuthor <- isAuthorOnPaper uid p
            return $ if isAuthor then Authorized else Unauthorized msg

getCurrentPhase :: Handler (Entity CurrentPhase)
getCurrentPhase = do
    phases <- runDB $ selectList [] [] 
    return $ case phases of
        [x] -> x
        _ -> error "There should only be one phase"

-- | Helper function to determine if a user can view reviews of a paper
isViewReviewAuthenticated :: PaperId -> Handler AuthResult
isViewReviewAuthenticated p = do
    pair <- maybeAuthPair
    let msg = "You do not have access to this page"
    case pair of
        Nothing -> return $ Unauthorized msg
        Just (uid, user) -> do 
            Entity _pid phase <- getCurrentPhase
            case currentPhasePhase phase of 
                Decision -> do
                    let pc = userPc user
                    isAuthor <- isAuthorOnPaper uid p
                    return $ if pc || isAuthor then Authorized else Unauthorized msg
                _ -> return $ if userPc user then Authorized else Unauthorized msg

-- | Access function to determine if a user can download the paper
isDownloadAuthenticated :: PaperId -> Handler AuthResult
isDownloadAuthenticated p = do
    pair <- maybeAuthPair
    let msg = "You do not have access to this page"
    case pair of
        Nothing -> return $ Unauthorized msg
        Just (uid, user) -> do 
            Entity _pid phase <- getCurrentPhase
            case currentPhasePhase phase of 
                Decision -> do
                    paper <- runDB $ get404 p
                    return $ if paperPcAccepted paper then Authorized else Unauthorized msg
                _ -> do
                        isReviewer <- isReviewerOnPaper uid p
                        isAuthor <- isAuthorOnPaper uid p
                        let pc = userPc user
                        return $ if isReviewer || isAuthor || pc 
                            then Authorized else Unauthorized msg

isReviewPaperAuthenticated :: ReviewId -> Handler AuthResult
isReviewPaperAuthenticated r = do
    pair <- maybeAuthPair
    let msg = "You do not have access to this page"
    case pair of
        Nothing -> return $ Unauthorized msg
        Just (uid, _user) -> do 
            isReviewer <- isReviewerForId uid r
            return $ if isReviewer then Authorized else Unauthorized msg

-- | Policy Helpers
-- Some of this was actually really annoying from a SWE perspective. Everything
-- here is included in "Import", which includes the definitions of App and
-- Handler. My DB class returns everything as a Handler, since I'm using 
-- runDB. This makes it so that I cannot use DB functions here, as it would
-- create an import loop.

-- | Helper function for determining if a user is the author on a paper.
isAuthorOnPaper :: UserId -> PaperId -> Handler Bool
isAuthorOnPaper uid p = do
    authors <- runDB $ selectList [AuthorPaper ==. p] []
    let authorIds = map (\(Entity _aid author) -> authorAuthorUser author) authors
    return $ elem uid authorIds

-- | Helper function for determining if a user is the reviewer for the given review id.
isReviewerForId:: UserId -> ReviewId -> Handler Bool
isReviewerForId uid r = do
    review <- runDB $ get404 r
    return $ uid == (reviewUser review)

-- | Helper function for determining if a user is a reviewer on a paper.
isReviewerOnPaper :: UserId -> PaperId -> Handler Bool
isReviewerOnPaper u p = do
    reviewEnts <- runDB $ selectList [ReviewPaper ==. p] []
    let reviewerIds = map (\(Entity _rid review) -> reviewUser review) reviewEnts 
    return $ elem u reviewerIds 

-- | Only authors and PC chair can see who is an author until decision phase.
canViewAuthors :: Entity User -> PaperId -> Handler Bool
canViewAuthors (Entity uid u) p = do 
    isAuthor <- isAuthorOnPaper uid p
    return $ isAuthor || (userPc u)

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
