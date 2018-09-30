{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}

module Steps where

import Path
import qualified Data.Text as T
import Control.Arrow
import Control.Funflow
import Control.Funflow.ContentStore as CS
import Control.Funflow.ContentHashable
import Control.Funflow.External.Nix as Nix

import Types

paramPages :: PagesFile -> ParamField
paramPages (PagesFile pagesFile) = ParamPath $ IPItem pagesFile

filterPages :: T.Text -> (Tools, PagesFile) ==> PagesFile
{-
filterPages filterPred = proc (tools, inputPages) -> do
    bin <- arr (toolBinary [relfile|trec-car-filter-pages|]) -< tools
    out <- Nix.nix f -< (bin, inputPages)
    returnA -< PagesFile out
  where
    f :: (Path Abs File, PagesFile) -> NixConfig
    f (bin, inputPages) =
        nixConfig "trec-car-filter-pages"
        [ paramPages inputPages
        , ParamText "-o", ParamOut
        , ParamText filterPred ]
-}
filterPages filterPred =
    invokeTool [relfile|trec-car-filter-pages|] mkArgs >>^ PagesFile
  where
    mkArgs pagesFile =
        [ paramPages pagesFile
        , ParamText "-o", ParamOut
        , ParamText filterPred ]

importPages :: (Tools, DumpFile) ==> PagesFile
importPages =
    invokeTool [relfile|trec-car-import-pages|] mkArgs >>^ PagesFile
  where
    mkArgs (DumpFile dump) =
        [ ParamPath $ IPItem dump, ParamText "-o", ParamOut ]

concatPages :: (Tools, [PagesFile]) ==> PagesFile
concatPages =
    invokeTool [relfile|trec-car-cat-pages|] mkArgs >>^ PagesFile
  where
    mkArgs pagesFiles =
        [ paramPages pagesFile
        | pagesFile <- pagesFiles
        ] ++ [ ParamText "-o", ParamOut ]

fillMetadata :: T.Text -> (Tools, PagesFile) ==> PagesFile
fillMetadata args =
    invokeTool [relfile|trec-car-fill-metadata|] mkArgs >>^ PagesFile
   where
     mkArgs pagesFile =
         [ ParamText args
         , paramPages pagesFile
         , ParamText "-o", ParamOut ]

invokeTool :: ContentHashable IO a
           => Path Rel File -> (a -> [ParamField])
           -> (Tools, a) ==> Item
invokeTool tool mkArgs = proc (tools, x) -> do
    bin <- arr (toolBinary tool) -< tools
    Nix.nix f -< (bin, x)
  where
    f (bin, x) =
        nixConfig (T.pack $ fromRelFile tool) (mkArgs x)


newtype Tools = Tools (Path Abs Dir)

trecCarTools :: () ==> Tools
trecCarTools = proc () -> do
    returnA -< undefined

toolBinary :: Path Rel File -> Tools -> Path Abs File
toolBinary tool (Tools dir) =
    dir </> [reldir|bin|] </> tool

nixConfig :: T.Text -> [ParamField] -> NixConfig
nixConfig cmd args = NixShellConfig
    { environment = Nix.PackageList [ "zip" ]
    , nixpkgsSource = Nix.NIX_PATH
    , command = cmd
    , args = args
    , env = []
    , stdout = NoOutputCapture
    }
