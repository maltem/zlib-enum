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

import Data.Word (Word8)
import Data.List (unfoldr)

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L

import Data.Enumerator
  ( Stream (..), Step (..), Iteratee (..), Enumerator, Enumeratee
  , ($$), returnI, yield, continue, joinI, run, run_ )
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.Enumerator.Text as ET
import qualified Data.Enumerator.Binary as EB

import Control.Exception (bracket)
import Control.Monad.Trans (MonadIO (..), liftIO)

import Codec.Zlib (WindowBits (..))
import qualified Codec.Zlib.Enum as Z

import Test.QuickCheck
import Test.QuickCheck.Monadic
import qualified Test.QuickCheck.Monadic as Q

-- import System.IO.Unsafe (unsafePerformIO)
import System.Process (system)
import System.Environment (getArgs)
import System.IO (IOMode (..), openFile, hClose)

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

-- | QuickCheck wants a Show instance for WindowBits
instance Show WindowBits where
  show (WindowBits n) = "WindowBits " ++ show n

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

-- | Compress and decompress a ByteString with given WindowBits,
-- piping the stream with an Enumeratee.
compressDecompress
  :: MonadIO m
  => Enumeratee ByteString ByteString m ByteString
  -> WindowBits
  -> [ByteString]
  -> m ByteString
compressDecompress enum win xs = do
  E.run_ $  E.enumList 1 xs
         $$ joinI $ Z.compress 7 win
         $$ joinI $ enum
         $$ joinI $ Z.decompress win
         $$ consume

decompressUnconsumed :: FilePath -> IO ByteString
decompressUnconsumed file = do
  E.run_ $  EB.enumFile file
         $$ joinI $ Z.decompress (WindowBits 31)
         $$ joinI $ unconsumed
         $$ consume

-- | Create uncompressed and compressed files for testing.
setupFiles :: FilePath -> IO ()
setupFiles file = bracket
  (do hDeco <- openFile file WriteMode
      hComp <- openFile (file ++ ".gz") WriteMode
      return (hDeco, hComp)
  )

  ( \(hDeco, hComp) -> do
    mapM_ hClose [hDeco, hComp]
  )

  $ \(hDeco, hComp) -> do
    -- list of random ByteStrings
    xs <- sample' (arbitrary :: Gen ByteString)
    -- create uncompresssed file
    run_ $  E.enumList 1 xs
         $$ EB.iterHandle hDeco
    -- create compressed file
    run_ $  E.enumList 1 xs
         $$ joinI $ Z.compress 7 (WindowBits 31)
         $$ EB.iterHandle hComp

-------------------------------------------------------------------------------
-- Properties
-------------------------------------------------------------------------------

prop_identity :: [ByteString] -> WindowBits -> Property
prop_identity xs win = monadicIO $ do
  ys <- Q.run $ compressDecompress (E.map id) win xs
  assert (B.concat xs == ys)

prop_unconsumed :: [ByteString] -> WindowBits -> Property
prop_unconsumed xs win = monadicIO $ do
  ys <- Q.run $ compressDecompress unconsumed win xs
  assert (B.concat xs == ys)

prop_files :: FilePath -> Property
prop_files file = monadicIO $ do
  xs <- Q.run $ B.readFile file
  ys <- Q.run $ decompressUnconsumed (file ++ ".gz")
  assert (xs == ys)


-------------------------------------------------------------------------------
-- IO
-------------------------------------------------------------------------------

main :: IO ()
main = do
  -- quickCheck prop_identity

  quickCheck prop_unconsumed

  let testFile = "zlib-enum-test-file"
  setupFiles testFile
  quickCheck (prop_files testFile)
