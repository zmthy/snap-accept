------------------------------------------------------------------------------
module Snap.Accept
    (
    -- * Branching
      accept
    , accepts

    -- * Header names
    , FromHeader (..)
    ) where

------------------------------------------------------------------------------
import           Control.Monad                   (join, (>=>))
import           Data.ByteString                 (ByteString)
import           Data.CaseInsensitive            (CI)
import           Data.Maybe                      (fromMaybe)
import           Data.Proxy                      (Proxy (..))
import           Network.HTTP.Media
import           Network.HTTP.Media.RenderHeader (renderHeader)
import           Snap.Core


------------------------------------------------------------------------------
-- | The class of values that represent some Accept* header in a request and
-- corresponding Content-* header in a response, such that the name of the
-- header can be retrieved from the type.
class (Accept a, RenderHeader a) => FromHeader a where
  -- | The name of the corresponding Accept* header for this type.
  requestName :: Proxy a -> CI ByteString
  -- | The name of the corresponding Content-* header for this type.
  responseName :: Proxy a -> CI ByteString

instance FromHeader MediaType where
  requestName _ = "Accept"
  responseName _ = "Content-Type"

instance FromHeader Language where
  requestName _ = "Accept-Language"
  responseName _ = "Content-Language"


------------------------------------------------------------------------------
-- | Runs a Snap monad only if the request's Accept* header allows for the
-- given media type. If accepted, the response's Content-* header is
-- automatically filled in.
accept :: (FromHeader a, MonadSnap m) => a -> m b -> m b
accept acc action =
    withAccept (proxyFor acc) (matchAccept [acc]) >>= maybe (run acc) run
  where
    run = flip runWithType action
    proxyFor :: a -> Proxy a
    proxyFor _ = Proxy


------------------------------------------------------------------------------
-- | Matches the Accept* header of the request to each of the media types in
-- the pairs of the given list, running the corresponding Snap monad in the
-- pair that is most desired by the client. If two or more results arise with
-- the same quality level and specificity, then the pair that appears earliest
-- in the list is matched. On any match, the response's Content-* header is
-- automatically filled in.
accepts :: (FromHeader a, MonadSnap m) => [(a, m b)] -> m b
accepts []   = pass
accepts dict =
    withAccept (proxyFor dict) (mapAccept dict') >>=
    fromMaybe (snd $ head dict')
  where
    dict' = map (join $ fmap . runWithType . fst) dict
    proxyFor :: [(a, m b)] -> Proxy a
    proxyFor _ = Proxy


------------------------------------------------------------------------------
-- | Parses the Accept* header from the request and, if successful, passes it
-- to the given function.
withAccept :: (FromHeader a, MonadSnap m)
           => Proxy a
           -> (ByteString -> Maybe b) -> m (Maybe b)
withAccept proxy f = getsRequest $ getHeader (requestName proxy) >=> f


------------------------------------------------------------------------------
-- | Runs the given Snap monad with the given Accept value's rendered form set
-- in the appropriate header of the response.
runWithType :: (FromHeader a, MonadSnap m) => a -> m b -> m b
runWithType acc action = do
    modifyResponse (setHeader (responseName (proxyFor acc)) $ renderHeader acc)
    action
  where
    proxyFor :: a -> Proxy a
    proxyFor _ = Proxy
