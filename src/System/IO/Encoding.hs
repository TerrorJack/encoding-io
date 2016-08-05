{-|
    This module provides encoding-aware file I/O operations. They are lifted and operate with any textual instance of 'IOData'. A strict read operation is also provided.
-}

module System.IO.Encoding (
  -- * Encoding-aware file I/O
    readFileWithEncoding
  , readFileWithEncoding'
  , writeFileWithEncoding
  -- * Convenient re-exports
  , TextEncoding
  , latin1
  , utf8
  , utf8_bom
  , utf16
  , utf16be
  , utf16le
  , utf32
  , utf32be
  , utf32le
  , localeEncoding
  , char8
) where

import Control.DeepSeq
import Control.Monad.Base
import Data.IOData
import System.IO

readFileWithEncoding :: (MonadBase IO m, IOData a) => TextEncoding -> FilePath -> m a
readFileWithEncoding enc p = liftBase $ do
    h <- openFile p ReadMode
    hSetEncoding h enc
    Data.IOData.hGetContents h

readFileWithEncoding' :: (MonadBase IO m, IOData a, NFData a) => TextEncoding -> FilePath -> m a
readFileWithEncoding' enc p = liftBase $ withFile p ReadMode $ \h -> do
    hSetEncoding h enc
    s <- Data.IOData.hGetContents h
    deepseq s $ pure s

writeFileWithEncoding :: (MonadBase IO m, IOData a) => TextEncoding -> FilePath -> a -> m ()
writeFileWithEncoding enc p s = liftBase $ withFile p WriteMode $ \h -> do
    hSetEncoding h enc
    Data.IOData.hPut h s
