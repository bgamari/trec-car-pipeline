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



mainFlow :: () ==> ()
mainFlow = proc () -> do
    dumps <- fetchWikimediaDump -< ()
    tools <- trecCarTools -< ()
    allRawPages <- mapA importPages -< zip (repeat tools) dumps
    rawPages <- concatPages -< (tools, allRawPages)

    -- 0.4: kick out non-content pages
    contentPages <- filterPages (T.concat ["(!(", prefixMustPreds, "))"]) -< (tools, rawPages)

    -- 0.5: Fill redirect metadata
    fixedRedirects <- fillMetadata "--redirect" -< (tools, contentPages)
    redirectedPages <- filterPages "(!is-redirect)" -< (tools, fixedRedirects)

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
