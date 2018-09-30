{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeOperators #-}

module FetchUrl where

import Path

import Control.Arrow
import Control.Funflow
import Control.Funflow.ContentHashable
import Control.Funflow.ContentStore as CS
import Control.Funflow.External.Nix

import Data.ByteString.Lazy (ByteString)
import Network.Wreq
import Control.Lens

fetchUrl :: String ==> ByteString
fetchUrl = stepIO $ \url -> do
    res <- get url
    return $ res ^. responseBody
