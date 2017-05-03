module Composite.Reflex.AuthWrapper
  ( NotAuthedDetail(Authing, FailedAuth), _Authing, _FailedAuth
  , AuthState, _NotAuthed, _Authed
  , AuthEvents(AuthEvents), _authLoggingIn, _authCancel, authLoggingIn, authCancel
  , authWrapper
  ) where

import Composite.CoRecord (Case'(Case'), Field, fieldPrism)
import Composite.Record (Rec((:&), RNil), (:->), getVal)
import Composite.Reflex (dynCase)
import Control.Lens (Prism', (<&>), _Left, _Right, _Wrapped, has, preview, review)
import Control.Lens.TH (makeLenses, makePrisms)
import Data.Default (Default, def)
import Data.Functor (($>))
import Data.Proxy (Proxy(Proxy))
import Reflex.Dom (Behavior, hold, switch, Dynamic, holdDyn, updated, Event, ffilter, fmapMaybe, leftmost, never, MonadWidget, Reflex)

-- |Detail about why the current state is @NotAuthed@. See 'AuthState' for more.
data NotAuthedDetail err
  = Authing
  | FailedAuth err
makePrisms ''NotAuthedDetail

-- FIXME prism boilerplate

-- |Not currently authenticated, with possible detail explaining why.
type FNotAuthed err = "not_authed" :-> Maybe (NotAuthedDetail err)

-- |Current authenticated with some context information only available while authenticated.
type FAuthed ctx = "authed" :-> ctx

-- |Not currently authenticated, with possible detail explaining why.
_NotAuthed :: forall err ctx. Prism' (Field (AuthState err ctx)) (Maybe (NotAuthedDetail err))
_NotAuthed = fieldPrism (Proxy @(FNotAuthed err)) . _Wrapped

-- |Current authenticated with some context information only available while authenticated.
_Authed :: forall err ctx. Prism' (Field (AuthState err ctx)) ctx
_Authed = fieldPrism (Proxy @(FAuthed ctx)) . _Wrapped

-- |State representing either being authenticated (@Authed@) or not (@NotAuthed@).
--
-- Being authenticated carries some context @ctx@, while being not authenticated has a reason why:
--
-- * @Nothing@ indicates there's no particular reason why - either fresh visit to the app or logged out.
-- * @Just Authing@ indicates that an authentication attempt is currently in progress, which can be cancelled by a logout.
-- * @Just (FailedAuth err)@ indicates an authentication attempt failed.
type AuthState err ctx = '[FNotAuthed err, FAuthed ctx]

-- |Carrier holding event streams related to authentication, provided by the not authed page to 'authWrapper' to control login and logout.
data AuthEvents t res = AuthEvents
  { _authLoggingIn :: Event t (Event t res)
  -- ^Event which indicates that a login attempt has begun, giving the event which will fire when the login attempt completes.
  , _authCancel :: Event t ()
  -- ^Event which indicates a cancel.
  }
makeLenses ''AuthEvents

instance Reflex t => Default (AuthEvents t res) where
  def = AuthEvents never never

-- |Wrap the main logged-in portion of the app so that users see the login screens when they don't have an active auth context.
authWrapper
  :: forall t m res err ctx a. MonadWidget t m
  => Event t ()
  -- ^Event to trigger a logout, e.g. from a menu
  -> (Dynamic t (Maybe (NotAuthedDetail err)) -> m (AuthEvents t res))
  -- ^The not logged in widget, which emits events to trigger log in and log out
  -> (res -> Either err ctx)
  -- ^Interpretation of the authentication result @res@ into either an @err@ or successful auth @ctx@.
  -> (Dynamic t ctx -> m a)
  -- ^The logged-in app router to use when there is an established auth context. The additional Event can trigger a logout.
  -> m (Dynamic t (Field (AuthState err ctx)), Event t (Maybe a))
  -- ^Event which fires any time the auth context is established or broken
authWrapper logOut notAuthedWidget interpretAuthResponse authedWidget = do
  rec
    -- Event fired when the current login attempt finishes.
    currentLoginAttempt :: Behavior t (Event t res) <-
      hold never $ -- no initial attempt to log in
        leftmost
          -- if a logout is triggered, cancel a login in progress
          [ loggedOut $> never
          -- stop trying to log in if a login finishes
          , ffilter (has _Authed) (updated currentAuthState) $> never
          -- when a new login attempt is trigged, use that
          , loggingIn
          ]

    -- Current state of authentication
    currentAuthState :: Dynamic t (Field (AuthState err ctx)) <-
      holdDyn (review _NotAuthed Nothing) $ -- start out not authenticated
        leftmost
          -- immediately clear the auth context if a logout occurs
          [ loggedOut $> review _NotAuthed Nothing
          -- otherwise, if there's a response from the auth attempt use it to switch states
          , switch currentLoginAttempt <&> either (review _NotAuthed . Just . FailedAuth) (review _Authed) . interpretAuthResponse
          -- and finally switch to authing if a new attempt is occuring
          , loggingIn $> review _NotAuthed (Just Authing)
          ]

    -- Result of switching based on the auth state to build the correct UI
    result :: Event t (Either (AuthEvents t res) a) <-
      dynCase currentAuthState
        $  Case' (fmap Left  . notAuthedWidget . fmap getVal)
        :& Case' (fmap Right . authedWidget    . fmap getVal)
        :& RNil

    loggingIn <- switch <$> hold never (fmapMaybe (preview $ _Left . authLoggingIn) result)
    cancel <- switch <$> hold never (fmapMaybe (preview $ _Left . authCancel) result)
    let loggedOut = leftmost [cancel, logOut]
        appResult = preview _Right <$> result

  pure (currentAuthState, appResult)

