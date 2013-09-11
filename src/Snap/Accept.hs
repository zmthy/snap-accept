------------------------------------------------------------------------------
module Snap.Accept
    ( accept
    , accepts
    ) where

------------------------------------------------------------------------------
import Control.Monad                 (join, liftM, (>=>))
import Data.Maybe                    (fromMaybe)
import Network.HTTP.Accept
import Network.HTTP.Accept.MediaType (toByteString)
import Snap.Core


------------------------------------------------------------------------------
-- | Runs a Snap monad only if the request's Accept header allows for the
-- given media type.  If accepted, the response's Content-Type header is
-- automatically filled in.
accept :: MonadSnap m => MediaType -> m a -> m a
accept mtype action =
    withAccept (matchAccept [mtype]) >>= maybe (run mtype) run
  where
    run = flip runWithType action


------------------------------------------------------------------------------
-- | Runs a Snap monad only if the request's Accept header allows for one of
-- the given media types.  If accepted, the expected type is passed to the
-- given function and the response's Content-Type header is automatically
-- filled in.
accepts :: MonadSnap m => [(MediaType, m a)] -> m a
accepts []   = pass
accepts dict = withAccept (mapAccept dict') >>= fromMaybe (snd $ head dict')
  where
    dict' = map (join $ fmap . runWithType . fst) dict


------------------------------------------------------------------------------
-- | Parses the Accept header from the request and, if successful, passes
-- it to the given function.
withAccept :: MonadSnap m => ([Quality MediaType] -> Maybe a) -> m (Maybe a)
withAccept f = liftM (getHeader "Accept" >=> parseAccept >=> f) getRequest


------------------------------------------------------------------------------
-- | Runs the given Snap monad with the given media type set in the
-- response's ContentType header.
runWithType :: MonadSnap m => MediaType -> m a -> m a
runWithType mtype action =
    modifyResponse (setContentType $ toByteString mtype) >> action

