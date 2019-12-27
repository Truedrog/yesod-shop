{-# OPTIONS_GHC -fno-warn-orphans #-}

module Foundation where

import Import.NoFoundation
import           Web.ServerSession.Backend.Persistent
import           Web.ServerSession.Frontend.Yesod
import           Network.Mail.Mime
import           Text.Shakespeare.Text          ( stext )
import           Text.Hamlet                    ( shamlet )
import           Text.Blaze.Html.Renderer.Utf8  ( renderHtml )
import Control.Monad.Logger        (LogSource)
import Yesod.Core.Types            (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import qualified Data.Text.Lazy.Encoding as TE
import AppType
import Routes

-- | Cookie name used for the sessions of this example app.
sessionCookieName :: Text
sessionCookieName = "SESSION"

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- | A convenient synonym for database access functions.
type DB a = forall  (m :: * -> *) . (MonadIO m) => ReaderT SqlBackend m a

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where

    errorHandler NotFound = redirectWith status307 HomeR
    errorHandler other = defaultErrorHandler other

    approot :: Approot App
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    makeSessionBackend = simpleBackend opts . SqlStorage . appConnPool
       where opts = setIdleTimeout     (Just $  5 * 60) -- 5  minutes
                 . setAbsoluteTimeout (Just $ 20 * 60) -- 20 minutes
                 . setCookieName      sessionCookieName

    yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
    yesodMiddleware = defaultYesodMiddleware

    authRoute :: App -> Maybe (Route App)
    authRoute _ = Just $ AuthR LoginR

    isAuthorized
        :: Route App  -- ^ The route the user is visiting.
        -> Bool       -- ^ Whether or not this is a "write" request.
        -> Handler AuthResult
    -- Routes not requiring authenitcation.
    isAuthorized DataR _ = return Authorized
    -- Default to Authorized for now.
    isAuthorized _ _ = return Authorized

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
    shouldLogIO app _source level =
        return $
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger :: App -> IO Logger
    makeLogger = return . appLogger

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB :: SqlPersistT Handler a -> Handler a
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
    getDBRunner :: Handler (DBRunner App, Handler ())
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest :: App -> Route App
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest :: App -> Route App
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer :: App -> Bool
    redirectToReferer _ = True

    authPlugins _ = [authEmail]

    authenticate cred = liftHandler $ runDB do
        x <- insertBy $ User (credsIdent cred) Nothing Nothing False
        return $ Authenticated
          case x of 
            Left (Entity userId _) -> userId
            Right userId -> userId

    authHttpManager = error "Email doesn't need an HTTP manager"
    
    -- authPlugins :: App -> [AuthPlugin App]
    -- authPlugins app = [authDummy | appAuthDummyLogin $ appSettings app]
    -- authPlugins app = []

-- | Access function to determine if a user is logged in.
isAuthenticated :: Handler AuthResult
isAuthenticated = do
    maybeAuthId >>= \case
        Nothing -> return $ Unauthorized "You must login to access this page"
        Just _  -> return $ Authorized

instance YesodAuthEmail App where
    type AuthEmailId App = UserId
    afterPasswordRoute _ = HomeR
    
    addUnverified email verkey = 
        liftHandler $ runDB $ insert $ User email Nothing (Just verkey) False

    sendVerifyEmail :: Email -> VerKey -> VerUrl -> AuthHandler site ()
    sendVerifyEmail email _ verurl = do 
        $logInfo $ "Copy/ Paste this URL in your browser:" <> verurl
        liftIO $ 
          renderSendMail 
            (emptyMail $ Address Nothing "noreply")
            { mailTo = [Address Nothing email]
            , mailHeaders = [("Subject", "Verify your email address")]
            , mailParts = [[textPart, htmlPart]]
            }
        where
            textPart = 
                Part
                { partType = "text/plain; charset=utf-8"
                , partEncoding = None
                , partDisposition = DefaultDisposition
                , partContent =
                  PartContent $ TE.encodeUtf8
                    [stext| 
                        Please confirm your email address by clicking on the link below.

                        #{verurl}

                        Thank you
                    |]
                , partHeaders = []
                }
            htmlPart =
                Part
                { partType = "text/html; charset=utf-8"
                , partEncoding = None
                , partDisposition = DefaultDisposition
                , partContent =
                  PartContent $ renderHtml
                    [shamlet| 
                       <p>Please confirm your email address by clicking on the link below.
                       <p>
                        <a href=#{verurl}>#{verurl}
                       <p>Thank you
                    |]
                , partHeaders = []
                }
   
    getVerifyKey = liftHandler . runDB . fmap (join . fmap userVerkey) . get
    needOldPassword _ = return False
    setVerifyKey uid key = liftHandler $ runDB $ update uid [UserVerkey =. Just key]
    verifyAccount uid = liftHandler $ runDB $ do
       mu <- get uid
       case mu of
         Nothing -> return Nothing
         Just u -> do
           update uid [UserVerified =. True, UserVerkey =. Nothing]
           return $ Just uid
    getPassword = liftHandler . runDB . fmap (join . fmap userPassword) . get
    setPassword uid pass = liftHandler $ runDB $ update uid [UserPassword =. Just pass]
    getEmailCreds email = liftHandler $ runDB $ do
        mu <- getBy $ UniqueUser email
        case mu of
            Nothing -> return Nothing
            Just (Entity uid u) -> 
                return $ 
                Just 
                  EmailCreds 
                  { emailCredsId = uid
                  , emailCredsAuthId = Just uid
                  , emailCredsStatus = isJust $ userPassword u
                  , emailCredsVerkey = userVerkey u
                  , emailCredsEmail  = email
                  }
    getEmail = liftHandler . runDB . fmap (fmap userEmail) . get

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage :: App -> [Lang] -> FormMessage -> Text
    renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager :: App -> Manager
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
