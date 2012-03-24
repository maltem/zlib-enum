{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-------------------------------------------------------------------------------
-- |
-- Module     : zlib-enum-test
-- Copyright  : (c) 2011 Malte Sommerkorn, Michael Snoyman, Paulo Tanimoto
-- License    : BSD3
--
-- Maintainer : Malte Sommerkorn <malte.sommerkorn@googlemail.com>
--
-------------------------------------------------------------------------------

module Main where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import Prelude

import Data.String ()

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

import Data.Enumerator
  ( Iteratee (..), Enumeratee
  , ($$), joinI, run_ )
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Binary as EB

import Control.Monad (foldM)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Exception (bracket)

import Codec.Zlib
import qualified Codec.Zlib.Enum as Z

import Test.QuickCheck
import Test.QuickCheck.Monadic
import qualified Test.QuickCheck.Monadic as Q

import System.IO (IOMode (..), openBinaryFile, hClose)

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

-- | QuickCheck wants a Show instance for WindowBits
instance Show WindowBits where
  show (WindowBits n) = "WindowBits " ++ show n
  show _              = "DefaultWindowBits" -- not exported from zlib

-- | Random values for WindowBits: 15 or 31
instance Arbitrary WindowBits where
  arbitrary = elements [WindowBits 15, WindowBits 31]

-- | Generate relatively large ByteStrings
instance Arbitrary B.ByteString where
    arbitrary = (B.pack . concat . replicate 1000) `fmap` arbitrary

-- | Generate relatively large Lazy ByteStrings
instance Arbitrary L.ByteString where
    arbitrary = (L.fromChunks . concat . replicate 100) `fmap` arbitrary

-------------------------------------------------------------------------------
-- Utility Functions
-------------------------------------------------------------------------------

-- | Iteratee that consumes the entire Stream as a strict ByteString.
consume :: Monad m => Iteratee ByteString m ByteString
consume = do
  xs <- EL.consume
  return $ B.concat xs

-- | Enumeratee that consumes only a few bytes.
unconsumed :: Monad m => Enumeratee ByteString ByteString m a
unconsumed = E.sequence $ do
  xs <- EB.take 100
  return $ B.concat $ L.toChunks xs

-- | Concatenate an Iteratee and a list of Enumeratees into an Iteratee.
concatWith :: Monad m => Iteratee a m b -> [Enumeratee a a m b] -> Iteratee a m b
concatWith = foldr f
  where
  f iter enums = joinI $ iter $$ enums

-- | Compress a list of ByteStrings
compress
  :: MonadIO m
  => WindowBits
  -> [ByteString]
  -> m ByteString
compress win xs =
  E.run_ $  E.enumList 1 xs
         $$ joinI $ Z.compress 7 win
         $$ consume

-- | Decompress a list of ByteStrings
decompress
  :: MonadIO m
  => WindowBits
  -> [ByteString]
  -> m ByteString
decompress win xs =
  E.run_ $  E.enumList 1 xs
         $$ joinI $ Z.decompress win
         $$ consume

-- | Compress and decompress without doing anything else.
compressDecompress
  :: MonadIO m
  => WindowBits
  -> [ByteString]
  -> m ByteString
compressDecompress win xs =
  E.run_ $  E.enumList 1 xs
         $$ joinI $ Z.compress 7 win
         $$ joinI $ Z.decompress win
         $$ consume

-- | The same, but for 'gzip', 'ungzip'
gzipUngzip :: MonadIO m => [ByteString] -> m ByteString
gzipUngzip xs =
  E.run_ $  E.enumList 1 xs
         $$ joinI $ Z.gzip
         $$ joinI $ Z.ungzip
         $$ consume

-- | Compress and decompress a ByteString with given WindowBits,
-- piping the stream with an Enumeratee.
compressDecompressWith
  :: MonadIO m
  => Enumeratee ByteString ByteString m ByteString
  -> WindowBits
  -> [ByteString]
  -> m ByteString
compressDecompressWith enum win xs =
  E.run_ $  E.enumList 1 xs
         $$ joinI $ Z.compress 7 win
         $$ joinI $ enum
         $$ joinI $ Z.decompress win
         $$ consume

-- | Compress a ByteString 'n' times and then decompress it 'n' times
compressDecompressMany
  :: MonadIO m
  => WindowBits
  -> Int
  -> [ByteString]
  -> m ByteString
compressDecompressMany win n xs =
  E.run_ $  E.enumList 1 xs
         $$ concatWith consume es
  where
  es = replicate m (Z.compress 7 win) ++ replicate m (Z.decompress win)
  m = 1 + (abs n `rem` 10) -- restrict n to [1, 10]

-- | Compress a [ByteString] to a file with an Enumeratee
compressFileWith
  :: Enumeratee ByteString ByteString IO ()
  -> WindowBits -> FilePath -> [ByteString] -> IO ()
compressFileWith enum win file xs = bracket
  (openBinaryFile file WriteMode)
  (hClose)
  $ \ h -> do
    run_ $  E.enumList 1 xs
         $$ joinI $ Z.compress 7 win
         $$ joinI $ enum
         $$ EB.iterHandle h

-- | Decompress from a file with an Enumeratee
decompressFileWith
  :: Enumeratee ByteString ByteString IO ByteString
  -> WindowBits -> FilePath -> IO ByteString
decompressFileWith enum win file =
  run_ $  EB.enumFile file
       $$ joinI $ Z.decompress win
       $$ joinI $ enum
       $$ consume

-- | Alternative implementation of compress for comparison
compressChunks :: WindowBits -> [ByteString] -> IO [ByteString]
compressChunks win xs = do
    def <- initDeflate 7 win
    gziped <- foldM (go' def) id xs
    gziped' <- go gziped (finishDeflate def)
    return $ gziped' []
    where
    go' def front bs = feedDeflate def bs >>= go front
    go front x = do
        y <- x
        case y of
            Nothing -> return front
            Just z -> go (front . (:) z) x

-- | Alternative implementation of decompress for comparison
decompressChunks :: WindowBits -> [ByteString] -> IO [ByteString]
decompressChunks win xs = do
    inf <- initInflate win
    ungziped <- foldM (go' inf) id xs
    final <- finishInflate inf
    return $ ungziped [final]
    where
    go' inf front bs = feedInflate inf bs >>= go front
    go front x = do
        y <- x
        case y of
            Nothing -> return front
            Just z -> go (front . (:) z) x

-------------------------------------------------------------------------------
-- Properties
-------------------------------------------------------------------------------

-- | Compare compresssion via lists and enumerator
prop_compress_compare :: WindowBits -> [ByteString] -> Property
prop_compress_compare win xs = monadicIO $ do
  chks <- Q.run $ B.concat `fmap` compressChunks win xs
  enum <- Q.run $ compress win xs
  assert $ enum == chks

-- | Compare decompression via lists and enumerator
prop_decompress_compare :: WindowBits -> [ByteString] -> Property
prop_decompress_compare win xs = monadicIO $ do
  comp <- Q.run $ compressChunks win xs
  chks <- Q.run $ B.concat `fmap` decompressChunks win comp
  enum <- Q.run $ decompress win comp
  assert $ enum == chks

-- | Check: bs == decompress (compress bs)
-- (That is, with separate Enumeratees)
prop_compress_decompress :: WindowBits -> [ByteString] -> Property
prop_compress_decompress win xs = monadicIO $ do
  cs <- Q.run $ compress win xs
  ys <- Q.run $ decompress win [cs]
  assert (B.concat xs == ys)

-- | Check: bs == compressDecompress bs
-- (That is, with a single Enumeratee)
prop_compress_decompress' :: WindowBits -> [ByteString] -> Property
prop_compress_decompress' win xs = monadicIO $ do
  ys <- Q.run $ compressDecompress win xs
  assert (B.concat xs == ys)

-- | Check: bs == gzipUngzip bs
-- (just to see if the default parameters aren't broken)
prop_gzip_ungzip :: [ByteString] -> Property
prop_gzip_ungzip xs = monadicIO $ do
  ys <- Q.run $ gzipUngzip xs
  assert (B.concat xs == ys)

-- | Check if using an Iteratee that consumes only a few bytes works
prop_unconsumed :: WindowBits -> [ByteString] -> Property
prop_unconsumed win xs = monadicIO $ do
  ys <- Q.run $ compressDecompressWith unconsumed win xs
  assert (B.concat xs == ys)

-- | Check if mapping the identity function doesn't affect anything
prop_map_id :: WindowBits -> [ByteString] -> Property
prop_map_id win xs = monadicIO $ do
  ys <- Q.run $ compressDecompressWith (EL.map id) win xs
  assert (B.concat xs == ys)

-- | Check if mapping 'reverse . reverse' doesn't affect anything
prop_map_revrev :: WindowBits -> [ByteString] -> Property
prop_map_revrev win xs = monadicIO $ do
  ys <- Q.run $ compressDecompressWith (EL.map $ B.reverse . B.reverse) win xs
  assert (B.concat xs == ys)

-- | Check if compressing and decompressing multiple times works
prop_many :: WindowBits -> Int -> [ByteString] -> Property
prop_many win n xs = monadicIO $ do
  ys <- Q.run $ compressDecompressMany win n xs
  assert (B.concat xs == ys)

-- | Check compressing and decompressing a file
prop_files_map_id :: FilePath -> WindowBits -> [ByteString] -> Property
prop_files_map_id file win xs = monadicIO $ do
  Q.run $ compressFileWith enum win file xs
  ys <- Q.run $ decompressFileWith enum win file
  assert (B.concat xs == ys)
  where
  enum = EL.map id

-- | Check compressing and decompressing a file with an Iteratee that
-- consumes only a few bytes
prop_files_unconsumed :: FilePath -> WindowBits -> [ByteString] -> Property
prop_files_unconsumed file win xs = monadicIO $ do
  Q.run $ compressFileWith unconsumed win file xs
  ys <- Q.run $ decompressFileWith unconsumed win file
  assert (B.concat xs == ys)

-------------------------------------------------------------------------------
-- Tests
-------------------------------------------------------------------------------

tests :: [Test]
tests = let testFile = "zlib-enum-test-file" in
  [ testGroup "compare"
    [ testProperty "compress_compressChunks" prop_compress_compare
    , testProperty "decompress_decompressChunks" prop_decompress_compare
    ]
  , testGroup "enumList"
    [ testProperty "compress_decompress" prop_compress_decompress
    , testProperty "compress_decompress'" prop_compress_decompress'
    , testProperty "gzip_ungzip" prop_gzip_ungzip
    , testProperty "unconsumed" prop_unconsumed
    , testProperty "map_id" prop_map_id
    , testProperty "map_revrev" prop_map_revrev
    , testProperty "many" prop_many
    ]
  , testGroup "enumFile"
    [ testProperty "files_map_id" (prop_files_map_id testFile)
    , testProperty "files_unconsumed" (prop_files_unconsumed testFile)
    ]
  ]

-------------------------------------------------------------------------------
-- IO
-------------------------------------------------------------------------------

main :: IO ()
main = defaultMain tests
