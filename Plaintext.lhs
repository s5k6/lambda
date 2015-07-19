> {-# LANGUAGE TemplateHaskell #-}

> module Plaintext where
>
> import Language.Haskell.TH
> import System.Process
> import System.Directory
> import System.IO.Error



Provide the entire file contents as one big string.

> stringOfFile :: FilePath -> Q Exp
> stringOfFile fp = runIO (readFile fp) >>= stringE



Export date ofcompilation

> compDate :: Q Exp
> compDate
>   = stringE . init
>     =<<
>     runIO (readProcess "date" ["+%a %Y-%b-%d %H:%M:%S %Z"] "")



OMFG: Try to snatch SVN revision with svn tools.  If not possile,
resort to REVISION file.  Fallback to UNKNOWN.

> svnRevision :: Q Exp
> svnRevision
>   = stringE =<< runIO f
>   where
>   f = (fromREVISION `catchIOError` (const fromSvnInfo))
>       `catchIOError` (const $ return "UNKNOWN")
>   fromSvnInfo = init <$> readProcess "svnversion" [] ""
>   fromREVISION = init <$> readFile "REVISION"



This generates a list of all help topics, one for each file in the
`help` subdirectory.  Only `:h list` and `:h primitives` are generated
at runtime.

> allHelp :: Q Exp
> allHelp
>   = do fs <- runIO $ filter ((/='.') . head) <$> getDirectoryContents "help"
>        cs <- mapM (stringOfFile . ("help/"++)) fs
>        let mfl = mkName "M.fromList"
>        return . AppE (VarE mfl) . ListE $ zipWith (\f c -> TupE [LitE (StringL f), c]) fs cs
