{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Exception ( displayException )

import Path
import Path.IO
import Control.Arrow
import Control.Arrow.Free
import Control.Funflow
import Control.Funflow.ContentHashable
import Control.Funflow.ContentStore as CS
import Control.Funflow.External.Nix
import Data.Aeson
import qualified Data.Text as T

import FetchUrl
import Glove
import WikimediaDump
import Steps

main :: IO ()
main = do
    cwd <- getCurrentDir
    r <- withSimpleLocalRunner (cwd </> [reldir|store|]) $ \run ->
      run mainFlow ()

    case r of
      Left err ->
        putStrLn $ "FAILED: " ++ displayException err
      Right out ->
        putStrLn $ "SUCCESS: " ++ show out

forbiddenHeadings =
    [ "see also"
    , "references"
    , "external links"
    , "notes"
    , "bibliography"
    , "gallery"
    , "publications"
    , "further reading"
    , "track listing"
    , "sources"
    , "cast"
    , "discography"
    , "awards"
    , "other"
    , "external links and references"
    , "notes and references"
    ]

mainFlow :: () ==> ()
mainFlow = proc () -> do
    tools <- trecCarTools -< ()
    glove <- fetchGloveEmbedding -< ()

    -- 0. Fetch dump
    let wiki = (WikiId "lbwiki", DumpDate "20180920")
    dumps <- fetchWikimediaDump -< wiki

    -- 0.1. Parse pages
    allRawPages <- mapA importPages -< zip (repeat tools) dumps
    rawPages <- concatPages -< (tools, allRawPages)

    -- 0.4. Kick out non-content pages
    contentPages <- filterPages (T.concat ["(!(", prefixMustPreds, "))"]) -< (tools, rawPages)

    -- 0.5. Fill redirect metadata
    fixedRedirects <- fillMetadata "--redirect" -< (tools, contentPages)
    redirectedPages <- filterPages "(!is-redirect)" -< (tools, fixedRedirects)

    -- 0.6. Fill disambiguation and in-link metadata
    unprocessedAll <- fillMetadata "--disambiguation" -< (tools, redirectedPages)

    -- 1. Drop non-article pages
    articles <- filterPages "(!is-disambiguation & !is-category)" -< (tools, unprocessedAll)
    unprocessedTrain <- filterPages "(train-set)" -< (tools, articles)

    -- 2. Drop administrative headings and category links
    processedArticles <- transformContent ("--lead --image --shortHeading --longHeading --shortpage" <> T.unwords [ "--forbidden " <> heading | heading <- forbiddenHeadings ]) -< (tools, articles)
    allParagraphs <- exportParagraphs -< (tools, processedArticles)

    -- 3. Drop duplicate paragraphs
    duplicateMapping <- minhashDuplicates -< (tools, (glove, allParagraphs))
    dedupArticles <- deduplicate -< (tools, (duplicateMapping, processedArticles))
    paragraphCorpus <- exportParagraphs -< (tools, dedupArticles)

    -- 3.1. Drop pages of forbidden categories
    filtered <-
        (let preds = undefined -- TODO
         in filterPages preds)
        -< (tools, dedupArticles)

    -- 4. Drop lead, images, long/short sections, articles with <3 sections
    base <- transformContent (T.unwords [ "--forbidden " <> heading | heading <- forbiddenHeadings ]) -< (tools, filtered)

    -- 5. Train/test split
    baseTest <- filterPages "(test-set)" -< (tools, base)
    baseTrain <- filterPages "(train-set)" -< (tools, base)

    -- 6. Split train into folds
    baseTrainFolds <- mapA filterFold -< zip (repeat tools) (zip (repeat baseTrain) [0..4])

    {-
    -- 7. Packaging
    readme <- mkReadme -< ()
    license <- mkLicense -< ()

    baseTrainFoldExports <- mapA exportAll -< baseTrainFolds
    trainPackage <- package -< [license, readme] <> baseTrainFoldExports

    foldPagesFile <- mapA filterFold -< zip (repeat redirectedPages) [0..4]
    paragraphFile <- mapA exportParagraphs -< foldPagesFile
    archive <- zipIt -< [paragraphFile, outlinesFile, readme]++qrels
    -}

    returnA -< ()

prefixMustPreds = T.intercalate " | "
      [ "name-has-prefix \"" <> category <> "\""
      | category <-
            [ "Category talk:"
            , "Talk:"
            , "File:"
            , "File talk:"
            , "Special:"
            , "User:"
            , "User talk:"
            , "Wikipedia talk:"
            , "Wikipedia:"
            , "Template:"
            , "Template talk:"
            , "Module:"
            , "Draft:"
            , "Help:"
            , "Book:"
            , "TimedText:"
            , "MediaWiki:"
            ]
      ]
