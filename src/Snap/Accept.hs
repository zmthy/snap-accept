------------------------------------------------------------------------------
-- | HTTP media type functionality as a complement to Snap's 'method' and
-- 'methods' functions.  Branches based on the value of the Accept or
-- Accept-Language header of the current request, automatically setting the
-- Content-Type or Content-Language header of the response as appropriate.
module Snap.Accept
    (
    -- * Branching
      accept
    , acceptMedia
    , acceptLanguage
    , accepts
    , acceptsMedia
    , acceptsLanguage

    -- * Accept types
    , MediaType
    , Language

    -- * Header names
    , FromHeader (..)
    ) where

import           Control.Monad                   (join)
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
  -- | The default header value to use if the header is absent.
  defaultValue :: Proxy a -> ByteString

instance FromHeader MediaType where
  requestName _ = "Accept"
  responseName _ = "Content-Type"
  defaultValue _ = "*/*"

instance FromHeader Language where
  requestName _ = "Accept-Language"
  responseName _ = "Content-Language"
  defaultValue _ = "*"


------------------------------------------------------------------------------
-- | Runs a Snap monad only if the request's Accept* header allows for the
-- given value.  If accepted, the response's Content-* header is automatically
-- filled in.
accept :: (FromHeader a, MonadSnap m) => a -> m b -> m b
accept a s =
    withAccept (proxyFor a) (matchAccept [a]) >>= maybe pass (`withHeader` s)
  where
    proxyFor :: a -> Proxy a
    proxyFor _ = Proxy

-- | The 'accept' function but specialised to 'MediaType'.
acceptMedia :: (MonadSnap m) => MediaType -> m a -> m a
acceptMedia = accept

-- | The 'accept' function but specialised to 'Language'.
acceptLanguage :: (MonadSnap m) => Language -> m a -> m a
acceptLanguage = accept


------------------------------------------------------------------------------
-- | Matches the Accept* header of the request to each of the values in the
-- pairs of the given list, running the corresponding Snap monad in the pair
-- that is most desired by the client.  If two or more results arise with the
-- same quality level and specificity, then the pair that appears earliest in
-- the list is matched.  On any match, the response's Content-* header is
-- automatically filled in.
accepts :: (FromHeader a, MonadSnap m) => [(a, m b)] -> m b
accepts d = withAccept (proxyFor d) (mapAccept d') >>= fromMaybe pass
  where
    d' = map (join $ fmap . withHeader . fst) d
    proxyFor :: [(a, m b)] -> Proxy a
    proxyFor _ = Proxy

-- | The 'accepts' function but specialised to 'MediaType'.
acceptsMedia :: (MonadSnap m) => [(MediaType, m a)] -> m a
acceptsMedia = accepts

-- | The 'accepts' function but specialised to 'Language'.
acceptsLanguage :: (MonadSnap m) => [(Language, m a)] -> m a
acceptsLanguage = accepts


------------------------------------------------------------------------------
-- | Parses the Accept* header from the request and, if successful, passes it
-- to the given function.
withAccept :: (FromHeader a, MonadSnap m)
           => Proxy a
           -> (ByteString -> Maybe b) -> m (Maybe b)
withAccept p f = getsRequest $ f . fromMaybe (defaultValue p) . getHeader name
  where
    name = requestName p


------------------------------------------------------------------------------
-- | Runs a Snap monad with the rendered value of an Content-* form set in the
-- appropriate header of the response.
withHeader :: (FromHeader a, MonadSnap m) => a -> m b -> m b
withHeader a m = modifyResponse (setHeader name (renderHeader a)) >> m
  where
    name = responseName (proxyFor a)
    proxyFor :: a -> Proxy a
    proxyFor _ = Proxy
