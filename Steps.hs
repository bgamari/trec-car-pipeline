{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Glove

paramPages :: PagesFile -> ParamField
paramPages (PagesFile pagesFile) = ParamPath $ IPItem pagesFile

filterPages :: T.Text -> (Tools, PagesFile) ==> PagesFile
filterPages filterPred =
    invokeTool [relfile|trec-car-filter-pages|] mkArgs >>^ PagesFile
  where
    mkArgs pagesFile =
        [ paramPages pagesFile
        , ParamText "-o", ParamOut
        , ParamText filterPred ]

filterFold :: Int -> (Tools, PagesFile) ==> PagesFile
filterFold i = filterPages ("()") -- TOOD

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

transformContent :: T.Text -> (Tools, PagesFile) ==> PagesFile
transformContent args =
    invokeTool [relfile|trec-car-transform-content|] mkArgs >>^ PagesFile
   where
     mkArgs pagesFile =
         [ ParamText args
         , paramPages pagesFile
         , ParamText "-o", ParamOut ]

doExport :: T.Text -> (Tools, PagesFile) ==> Item
doExport mode =
    invokeTool [relfile|trec-car-export|] mkArgs
  where
    mkArgs pagesFile =
        [ ParamText mode, paramPages pagesFile ]

exportOutlines :: (Tools, PagesFile) ==> OutlinesFile
exportOutlines = doExport "outlines" >>^ OutlinesFile

exportParagraphs :: (Tools, PagesFile) ==> ParagraphsFile
exportParagraphs = doExport "paragraphs" >>^ ParagraphsFile

newtype DuplicateMapping = DuplicateMapping Item
                         deriving (ContentHashable IO)

minhashDuplicates :: (Tools, (GloveEmbedding, PagesFile)) ==> DuplicateMapping
minhashDuplicates =
    invokeTool [relfile|trec-car-minhash-duplicates|] mkArgs >>^ DuplicateMapping
   where
     mkArgs (glove, pages) =
         [ ParamText "--embeddings", ParamPath $ IPItem $ getGloveEmbedding glove
         , ParamText "-t", ParamText "0.9"
         , ParamText "--projections", ParamText "12"
         , ParamText "--output", ParamOut
         , ParamText "-c", ParamOut
         , paramPages pages
         , ParamText "+RTS -N50 -A64M -s -RTS"
         ]

deduplicate :: (Tools, (DuplicateMapping, PagesFile)) ==> PagesFile
deduplicate = proc (tools, (dupMapping, pages)) -> do
    dupTable <- buildDupTable -< (tools, dupMapping)
    pages' <- rewriteDuplicates -< (tools, (dupMapping, dupTable, pages))
    returnA -< PagesFile pages'
  where
    buildDupTable = invokeTool [relfile|trec-car-duplicates-rewrite-table|]
        $ \dupMapping ->
            [ ParamText "--output", ParamOut
            , ParamText "-d", paramDupMapping dupMapping
            ]
    rewriteDuplicates = invokeTool [relfile|trec-car-rewrite-duplicates|]
        $ \(dupMapping, dupTable, pages) ->
            [ ParamText "--output", ParamOut
            , ParamText "-d", paramDupMapping dupMapping
            , paramPages pages
            ]

    paramDupMapping (DuplicateMapping d) = ParamPath $ IPItem d

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
