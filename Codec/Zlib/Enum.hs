module Codec.Zlib.Enum (
    -- * Enumeratees
    compress, decompress, gzip, ungzip,
    -- * Re-exported from zlib-bindings
    WindowBits (..), defaultWindowBits, ZlibException
) where

import Codec.Zlib
import Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)

-- | Gzip compression with default parameters.
gzip :: MonadIO m => Enumeratee ByteString ByteString m a
gzip = compress 1 (WindowBits 31)

-- | Gzip decompression with default parameters.
ungzip :: MonadIO m => Enumeratee ByteString ByteString m a
ungzip = decompress (WindowBits 31)

-- |
-- Decompress (inflate) a stream of 'ByteString's. For example:
--
-- >    run $ enumFile "test.z" $$ decompress defaultWindowBits $$ printChunks True

decompress
    :: MonadIO m
    => WindowBits -- ^ Zlib parameter (see the zlib-bindings package as well as the zlib C library)
    -> Enumeratee ByteString ByteString m a
decompress config inner = do
    inf <- liftIO $ initInflate config
    decompress' inf inner

decompress' :: MonadIO m => Inflate -> Enumeratee ByteString ByteString m b
decompress' inf (Continue k) = do
    x <- EL.head
    case x of
        Nothing -> do
            chunk <- liftIO $ finishInflate inf
            lift $ runIteratee $ k $ Chunks [chunk]
        Just bs -> do
            chunks <- liftIO $ (feedInflate inf bs >>= callback)
            step <- lift $ runIteratee $ k $ Chunks chunks
            decompress' inf step
decompress' _ step = return step

-- |
-- Compress (deflate) a stream of 'ByteString's. The 'WindowBits' also control
-- the format (zlib vs. gzip).

compress
    :: MonadIO m
    => Int         -- ^ Compression level
    -> WindowBits  -- ^ Zlib parameter (see the zlib-bindings package as well as the zlib C library)
    -> Enumeratee ByteString ByteString m a
compress level config inner = do
    def <- liftIO $ initDeflate level config
    compress' def inner

compress' :: MonadIO m => Deflate -> Enumeratee ByteString ByteString m b
compress' def (Continue k) = do
    x <- EL.head
    case x of
        Nothing -> do
            chunks <- liftIO $ callback (finishDeflate def)
            lift $ runIteratee $ k $ Chunks chunks
        Just bs -> do
            chunks <- liftIO $ (feedDeflate def bs >>= callback)
            step <- lift $ runIteratee $ k $ Chunks chunks
            compress' def step
compress' _ step = return step

callback :: Monad m => m (Maybe a) -> m [a]
callback pop = go id where
    go front = do
       x <- pop
       case x of
           Nothing -> return $ front []
           Just y -> go (front . (:) y)
