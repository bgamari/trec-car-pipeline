{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Control.Funflow.ContentStore
import Control.Funflow.ContentHashable

--newtype PagesFile = PagesFile (Content File)
--newtype OutlinesFile = OutlinesFile (Content File)
--newtype ParagraphsFile = ParagraphsFile (Content File)

newtype DumpFile = DumpFile Item
                 deriving (ContentHashable IO)
newtype PagesFile = PagesFile Item
                  deriving (ContentHashable IO)
newtype OutlinesFile = OutlinesFile Item
                     deriving (ContentHashable IO)
newtype ParagraphsFile = ParagraphsFile Item
                       deriving (ContentHashable IO)
