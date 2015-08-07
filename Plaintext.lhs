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



Export information about the compilation process

> compDate :: Q Exp
> compDate
>   = stringE . init
>     =<<
>     runIO (readProcess "date" ["+%a %Y-%b-%d %H:%M:%S %Z"] "")



OMFG: Try to snatch a revision number.  Try git first.  If that fails,
resort to the REVISION file.  Then fall back to UNKNOWN.  Subversion
is not expected to see this code ever again...

> getRevision :: Q Exp
> getRevision
>   = stringE =<< runIO f
>   where
>   f = fromGitDescribe `catchIOError` (const $ fromREVISION `catchIOError` (const $ return "UNKNOWN"))
>   fromGitDescribe = init <$> readProcess "git" ["describe"] ""
>   -- fromSvnInfo = init <$> readProcess "svnversion" [] ""
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
