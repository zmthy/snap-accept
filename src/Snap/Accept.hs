{-# LANGUAGE OverloadedStrings #-}

module Snap.Accept
    ( accept
    , accepts
    ) where

------------------------------------------------------------------------------
import           Control.Monad ((>=>), liftM)
import           Data.ByteString.UTF8 (fromString)
import qualified Data.CaseInsensitive as CI
import           Network.HTTP.Accept (MediaType, match, parseAccepts)
import           Snap.Core


------------------------------------------------------------------------------
-- | Runs a 'Snap' monad action only if the request's Accept header allows for
-- the given media type.  If accepted, the response's Content-Type header is
-- automatically filled in.
accept :: MonadSnap m => MediaType -> m a -> m a
accept t action = accepts [t] $ const action


------------------------------------------------------------------------------
-- | Runs a 'Snap' monad action only if the request's Accept header allows for
-- one of the given media types.  If accepted, the expected type is passed to
-- the given function and the response's Content-Type header is automatically
-- filled in.
accepts :: MonadSnap m => [MediaType] -> (MediaType -> m a) -> m a
accepts []         _ = pass
accepts ts@(t : _) f = liftM (getHeader $ CI.mk "Accept") getRequest >>=
    maybe (g t) (maybe pass g . (parseAccepts >=> match ts))
  where
    g u = modifyResponse (setContentType . fromString $ show u) >> f u

