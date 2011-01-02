module Codec.Zlib.Enum (
    -- * Enumeratees
    compress, decompress,
    -- * Re-exported from zlib-bindings 
    WindowBits
) where

import Codec.Zlib
import Data.Enumerator
import Control.Monad.Trans (MonadIO, liftIO)
import Data.ByteString (ByteString)
import Control.Monad (join)

-- |
-- Decompress (inflate) a stream of 'ByteString's. For example:
--      
-- >    run $ enumFile "test.z" $$ decompress defaultWindowBits $$ printChunks True

decompress :: MonadIO m
    => WindowBits -- ^ Zlib parameter (see the zlib-bindings package as well as the zlib C library)
    -> Enumeratee ByteString ByteString m ()
decompress config step0 = do
    inflate <- liftIO $ initInflate config
    checkDone (continue . goInflate inflate) step0

-- |
-- Compress (deflate) a stream of 'ByteString's. The 'WindowBits' also control
-- the format (zlib vs. gzip).

compress :: MonadIO m
    => Int        -- ^ Compression level
    -> WindowBits -- ^ Zlib parameter (see the zlib-bindings package as well as the zlib C library)
    -> Enumeratee ByteString ByteString m ()
compress level config step0 = do
    deflate <- liftIO $ initDeflate level config
    checkDone (continue . goDeflate deflate) step0

goInflate :: MonadIO m
    =>  Inflate
        --  ^ Zlib state
    ->  (Stream ByteString -> Iteratee ByteString m b)
        --  ^ Continuation to be called on the next chunk of decompressed data
    ->  (Stream ByteString)
        --  ^ Compressed data
    ->  Iteratee ByteString m (Step ByteString m b)

goInflate inflate k stream = case stream of
    EOF             -> do lastChunk <- liftIO $ finishInflate inflate
                          return $$ k (Chunks [lastChunk])
    (Chunks [])     -> continue (goInflate inflate k)
    (Chunks (x:xs)) -> let cont k'      = goInflate inflate k' (Chunks xs)
                           inflateOne   = withInflateInput inflate x (return . callback k)
                       in checkDone cont $$ joinIO inflateOne

-- A callback function for withInflateInput / withDeflateInput
callback :: MonadIO m =>
    (Stream a -> Iteratee a m b) -> IO (Maybe a) -> m (Step a m b)

callback k pop = maybe done more =<< liftIO pop
  where
    done   = return (Continue k)
    more y = do step <- runIteratee (k (Chunks [y]))
                case step of
                     Continue k'    -> callback k' pop
                     other          -> return other

-- testInflate = do
--     h <- openBinaryFile "test-out" WriteMode
--     run $ enumFile "test.z"
--            $$ enumInflate defaultWindowBits
--            $$ iterHandle h
--     hClose h
-- 
-- testDeflate = do
--     h <- openBinaryFile "test.z" WriteMode
--     run $ enumFile "test"
--            $$ enumDeflate 7 defaultWindowBits
--            $$ iterHandle h
--     hClose h

-- Conversion utility
joinIO :: MonadIO m => IO (m (Step a m b)) -> Iteratee a m b
joinIO = Iteratee . join . liftIO

goDeflate :: MonadIO m
    =>  Deflate
    --  ^ Zlib state
    ->  (Stream ByteString -> Iteratee ByteString m b)
    --  ^ Continuation to be called on the next chunk of compressed data
    ->  (Stream ByteString)
    --  ^ Uncompressed data
    ->  Iteratee ByteString m (Step ByteString m b)

goDeflate deflate k stream = case stream of
    EOF             -> let callFinish = finishDeflate deflate (return . callback k)
                       in return $$ joinIO callFinish
    (Chunks [])     -> continue (goDeflate deflate k)
    (Chunks (x:xs)) -> let cont k'      = goDeflate deflate k' (Chunks xs)
                           deflateOne   = withDeflateInput deflate x (return . callback k)
                       in checkDone cont $$ joinIO deflateOne
