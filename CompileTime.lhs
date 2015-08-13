> {-# LANGUAGE TemplateHaskell #-}

> module CompileTime where

> import Templates as T
> import qualified Data.Map as M

> revision :: String
> revision = $(T.getRevision)

> date :: String
> date = $(T.compDate)

> help :: M.Map String String
> help = $(T.allHelp)
