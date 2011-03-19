module Codec.Zlib.Enum (
    -- * Enumeratees
    compress, decompress,
    -- * Re-exported from zlib-bindings
    WindowBits, defaultWindowBits
) where

import Codec.Zlib
import Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import Control.Monad.Trans (MonadIO, liftIO, lift)
import Data.ByteString (ByteString)
import Control.Monad (join)


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
            chunks <- liftIO $ withInflateInput inf bs $ go id
            step <- lift $ runIteratee $ k $ Chunks chunks
            decompress' inf step
    where
    go front pop = do
        x <- pop
        case x of
            Nothing -> return $ front []
            Just y -> go (front . (:) y) pop
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
            chunks <- liftIO $ finishDeflate def $ go id
            lift $ runIteratee $ k $ Chunks chunks
        Just bs -> do
            chunks <- liftIO $ withDeflateInput def bs $ go id
            step <- lift $ runIteratee $ k $ Chunks chunks
            compress' def step
    where
    go front pop = do
        x <- pop
        case x of
            Nothing -> return $ front []
            Just y -> go (front . (:) y) pop
compress' _ step = return step


-- testInflate = do
--     h <- openBinaryFile "test-out" WriteMode
--     run $ enumFile "test.z"
--            $$ decompress defaultWindowBits
--            $$ iterHandle h
--     hClose h
--
-- testDeflate = do
--     h <- openBinaryFile "test.z" WriteMode
--     run $ enumFile "test"
--            $$ compress 7 defaultWindowBits
--            $$ iterHandle h
--     hClose h
