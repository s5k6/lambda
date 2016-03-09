synopsis: This module contains constants that shall be determined at
    compile time, using the TemplateHaskell machanism.
author: Stefan Klinger <http://stefan-klinger.de>

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
