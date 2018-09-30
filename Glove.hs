{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}

module Glove where

import Path
import Control.Arrow
import Control.Funflow
import Control.Funflow.ContentHashable
import Control.Funflow.ContentStore as CS

import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text.Lazy.Encoding as TL

import FetchUrl
import Utils

newtype GloveEmbedding = GloveEmbedding { getGloveEmbedding :: Item }
                       deriving (ContentHashable IO)

fetchGloveEmbedding :: () ==> GloveEmbedding
fetchGloveEmbedding = proc () -> do
    fixed <- fixEncoding
        <<< fetchUrl -< gloveUrl
    path <- writeByteString -< (fixed, [relfile|glove|])
    arr (GloveEmbedding . contentItem) -< path
  where
    gloveUrl = "http://nlp.stanford.edu/data/glove.6B.zip"

    fixEncoding :: BSL.ByteString ==> BSL.ByteString
    fixEncoding = arr $
        BSL.unlines . mapMaybe decodeLine . BSL.lines
      where
        decodeLine :: BSL.ByteString -> Maybe BSL.ByteString
        decodeLine =
            either (const Nothing) (Just . TL.encodeUtf8)
            . TL.decodeUtf8'
