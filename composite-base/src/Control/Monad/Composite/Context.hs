{-# LANGUAGE UndecidableInstances #-}
-- Required to make passthrough instances for MonadContext for things like ReaderT work as they do not satisfy the functional dependency | m -> c

-- |Module with a `ReaderT` style monad specialized to holding a record.
module Control.Monad.Composite.Context
  ( ContextT(ContextT, runContextT), withContext, mapContextT
  , MonadContext(askContext, localContext), asksContext, askField
  ) where

import BasicPrelude
import Composite.Record (Record)
import Control.Lens (Lens', view)
import Control.Monad.Cont.Class (MonadCont(callCC))
import Control.Monad.Error.Class (MonadError(throwError, catchError))
import Control.Monad.Fail (MonadFail)
import qualified Control.Monad.Fail as MonadFail
import Control.Monad.Fix (MonadFix(mfix))
import Control.Monad.Reader.Class (MonadReader(local, ask, reader))
import Control.Monad.RWS.Class (MonadRWS)
import Control.Monad.State.Class (MonadState(get, put, state))
import Control.Monad.Writer.Class (MonadWriter(writer, tell, listen, pass))

-- |Class of monad (stacks) which have context reading functionality baked in. Similar to 'Control.Monad.Reader.MonadReader' but can coexist with a
-- another monad that provides 'Control.Monad.Reader.MonadReader' and requires the context to be a record.
class Monad m => MonadContext (c :: [*]) m | m -> c where
  -- |Fetch the context record from the environment.
  askContext :: m (Record c)

  -- |Run some action which has the same type of context with the context modified.
  localContext :: (Record c -> Record c) -> m a -> m a

-- |Project some value out of the context using a function.
asksContext :: MonadContext c m => (Record c -> a) -> m a
asksContext f = f <$> askContext

-- |Project some value out of the context using a lens (typically a field lens).
askField :: MonadContext c m => Lens' (Record c) a -> m a
askField = asksContext . view

-- |Monad transformer which adds an implicit environment which is a record. Isomorphic to @ReaderT (Record c) m@.
newtype ContextT (c :: [*]) (m :: (* -> *)) a = ContextT { runContextT :: Record c -> m a }

-- |Permute the current context with a function and then run some action with that modified context.
withContext :: (c' -> c) -> ContextT c a -> ContextT c' a
withContext f action = ContextT $ \ c' -> runContextLoggingT action (f c')

-- |Transform the monad underlying a 'ContextT' using a natural transform.
mapContextT :: (m a -> n b) -> ContextT c m a -> ContextT c n b
mapContextT f m = ContextT $ f . runContextT m

instance Monad m => MonadCtxLogger c (ContextLoggingT c m) where
  context = ContextLoggingT pure

instance MonadCtxLogger c m => MonadCtxLogger c (ReaderT r m) where
  context = ReaderT $ const context

instance Functor m => Functor (ContextLoggingT c m) where
  fmap f clt = ContextLoggingT $ fmap f . runContextLoggingT clt

instance Applicative m => Applicative (ContextLoggingT c m) where
  pure = ContextLoggingT . const . pure
  cltab <*> clta = ContextLoggingT $ \ r -> runContextLoggingT cltab r <*> runContextLoggingT clta r

instance Alternative m => Alternative (ContextLoggingT c m) where
  empty = ContextLoggingT . const $ empty
  m <|> n = ContextLoggingT $ \ r -> runContextLoggingT m r <|> runContextLoggingT n r

instance Monad m => Monad (ContextLoggingT c m) where
  clt >>= k = ContextLoggingT $ \ ctx -> do
    a <- runContextLoggingT clt ctx
    runContextLoggingT (k a) ctx

  fail = ContextLoggingT . const . fail

instance MonadIO m => MonadIO (ContextLoggingT c m) where
  liftIO = lift . liftIO

instance MonadTrans (ContextLoggingT c) where
  lift = ContextLoggingT . const

instance MonadTransControl (ContextLoggingT c) where
  type StT (ContextLoggingT c) a = a
  liftWith f = ContextLoggingT $ \ r -> f $ \ t -> runContextLoggingT t r
  restoreT = ContextLoggingT . const

instance MonadBase b m => MonadBase b (ContextLoggingT c m) where
  liftBase = ContextLoggingT . const . liftBase

instance MonadBaseControl b m => MonadBaseControl b (ContextLoggingT c m) where
  type StM (ContextLoggingT c m) a = StM m a
  restoreM = ContextLoggingT . const . restoreM
  liftBaseWith f =
    ContextLoggingT $ \ c ->
      liftBaseWith $ \ runInBase ->
        f (runInBase . ($ c) . runContextLoggingT)

instance MonadReader r m => MonadReader r (ContextLoggingT c m) where
  ask    = lift ask
  local  = mapContextLoggingT . local
  reader = lift . reader

instance MonadWriter w m => MonadWriter w (ContextLoggingT c m) where
  writer = lift . writer
  tell   = lift . tell
  listen = mapContextLoggingT listen
  pass   = mapContextLoggingT pass

instance MonadState s m => MonadState s (ContextLoggingT c m) where
  get   = lift get
  put   = lift . put
  state = lift . state

instance MonadRWS r w s m => MonadRWS r w s (ContextLoggingT c m)

instance MonadFix m => MonadFix (ContextLoggingT c m) where
  mfix f = ContextLoggingT $ \ r -> mfix $ \ a -> runContextLoggingT (f a) r

instance MonadFail m => MonadFail (ContextLoggingT c m) where
  fail = lift . MonadFail.fail

instance MonadError e m => MonadError e (ContextLoggingT c m) where
  throwError = lift . throwError
  catchError m h = ContextLoggingT $ \ r -> catchError (runContextLoggingT m r) (\ e -> runContextLoggingT (h e) r)

instance MonadPlus m => MonadPlus (ContextLoggingT c m) where
  mzero = lift mzero
  m `mplus` n = ContextLoggingT $ \ r -> runContextLoggingT m r `mplus` runContextLoggingT n r

instance MonadCont m => MonadCont (ContextLoggingT c m) where
  callCC f = ContextLoggingT $ \ r -> callCC $ \ c -> runContextLoggingT (f (ContextLoggingT . const . c)) r
