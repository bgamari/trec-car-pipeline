{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module WikimediaDump where

import Path
import Control.Arrow
import Control.Arrow.Free
import Control.Funflow
import Control.Funflow.ContentStore as CS
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import FetchUrl
import Types
import Utils

newtype WikiId = WikiId String
newtype DumpDate = DumpDate String

fetchDumpStatus :: (WikiId, DumpDate) ==> DumpStatus
fetchDumpStatus = proc (WikiId wikiId, DumpDate dumpDate) -> do
    let statusUrl = "https://dumps.wikimedia.org/"<>wikiId<>"/"<>dumpDate<>"/dumpstatus.json"
    statusFile <- fetchUrl -< statusUrl
    Right x <- readYaml <<< writeByteString -< (statusFile, [relfile|dump-status.json|])
    returnA -< x

data DumpStatus = DumpStatus { articleFiles :: [JobFile]
                             }

data JobFile = JobFile { fileName :: Path Rel File
                       , fileUrl  :: String
                       , fileSha1 :: T.Text
                       , fileSize :: Int
                       }

instance FromJSON DumpStatus where
    parseJSON = withObject "dump status" $ \o -> do
        jobs <- o .: "jobs"
        articles <- jobs .: "articlesdump"
        files <- articles .: "files"
        files' <- mapM file (HM.toList files)
        return $ DumpStatus files'
      where
        file (fname, o) =
            JobFile <$> either (fail . show) pure (parseRelFile fname)
                    <*> o .: "url"
                    <*> o .: "sha1"
                    <*> o .: "size"

mirrorRoot = "https://dumps.wikimedia.your.org"

fetchDumpFile :: JobFile ==> Content File
fetchDumpFile = proc f -> do
    bs <- fetchUrl -< mirrorRoot<>fileUrl f
    writeByteString -< (bs, fileName f)

fetchWikimediaDump :: (WikiId, DumpDate) ==> [DumpFile]
fetchWikimediaDump = proc wiki -> do
    status <- fetchDumpStatus -< wiki
    mapA fetchDumpFile -< articleFiles status
    returnA -< [] -- TODO
