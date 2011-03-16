module Codec.Zlib.Enum (
    -- * Enumeratees
    compress, decompress,
    -- * Re-exported from zlib-bindings 
    WindowBits, defaultWindowBits
) where

import Codec.Zlib
import Data.Enumerator as E
import Control.Monad.Trans (MonadIO, liftIO, lift)
import Data.ByteString (ByteString)
import Control.Monad (join)

joinIO :: MonadIO m => IO (m (Step a m b)) -> Iteratee a m b
joinIO = Iteratee . join . liftIO

enumLoop :: Monad m =>
     ((Stream b -> Iteratee b m c) -> Iteratee b m c)
     -> (a -> (Stream b -> Iteratee b m c) -> Iteratee b m c)
     -> Enumeratee a b m c
enumLoop done more = checkDone loop where
    loop k = do maybe_x <- E.head
                case maybe_x of
                     Nothing -> return $$ done k
                     Just x  -> checkDone loop $$ more x k

-- |
-- Decompress (inflate) a stream of 'ByteString's. For example:
--      
-- >    run $ enumFile "test.z" $$ decompress defaultWindowBits $$ printChunks True

decompress :: MonadIO m
    => WindowBits -- ^ Zlib parameter (see the zlib-bindings package as well as the zlib C library)
    -> Enumeratee ByteString ByteString m a
decompress config inner = do
    fzstr <- liftIO $ initInflate config
    decompress' fzstr inner

decompress' :: MonadIO m => Inflate -> Enumeratee ByteString ByteString m b
decompress' fzstr (Continue k) = do
    x <- E.head
    case x of
        Nothing -> do
            chunk <- liftIO $ finishInflate fzstr
            lift $ runIteratee $ k $ Chunks [chunk]
        Just bs -> do
            chunks <- liftIO $ withInflateInput fzstr bs $ go id
            step <- lift $ runIteratee $ k $ Chunks chunks
            decompress' fzstr step
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

compress :: MonadIO m
    => Int        -- ^ Compression level
    -> WindowBits -- ^ Zlib parameter (see the zlib-bindings package as well as the zlib C library)
    -> Enumeratee ByteString ByteString m a
compress level config step0 = do
    deflate <- liftIO $ initDeflate level config
    let done k    = joinIO $ finishDeflate deflate (return . callback k)
        more x k  = joinIO $ withDeflateInput deflate x (return . callback k)
    enumLoop done more step0

-- A callback function for withInflateInput / withDeflateInput
callback :: MonadIO m =>
    (Stream a -> Iteratee a m b) -> IO (Maybe a) -> m (Step a m b)

callback k pop = runIteratee . k . Chunks =<< liftIO (go id)
  where
  go front = do
    x <- pop
    case x of
      Nothing -> return $ front []
      Just y  -> go (front . (:) y)

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
