{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeOperators #-}

module Utils where

import Path
import Control.Arrow
import Control.Funflow
import Control.Funflow.ContentStore as CS

import qualified Data.ByteString.Lazy.Char8 as BSL

readByteString :: Content File ==> BSL.ByteString
readByteString = getFromStore $ BSL.readFile . fromAbsFile

writeByteString :: (BSL.ByteString, Path Rel File) ==> Content File
writeByteString = putInStoreAt $ \path x -> BSL.writeFile (fromAbsFile path) x
