> {-# LANGUAGE TemplateHaskell #-}

> module Plaintext where
>
> import Language.Haskell.TH
> import System.Process
> import System.Directory
> import System.IO.Error
> import Helper



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
>   = stringE =<< runIO getInfo
>   where
>   first es d = foldr (\e e' -> e `catchIOError` const e') d es
>   getInfo
>     = do r <- first [ trim <$> readProcess "git" ["describe"] ""
>                       -- , rmTrailingNl <$> readProcess "svnversion" [] ""
>                     , trim <$> readFile "REVISION"
>                     ] $ error "Unknown revision: Not a git repo, and no \
>                               \`REVISION` file found."
>          p <- first [ trim <$> readProcess "git" ["diff", "--shortstat", "HEAD"] ""
>                     ] $ return ""
>          return $ length p == 0 ? r $ r++" ("++p++")"


This generates a list of all help topics, one for each file in the
`help` subdirectory.  Only `:h list` and `:h primitives` are generated
at runtime.

> allHelp :: Q Exp
> allHelp
>   = do fs <- runIO $ filter ((/='.') . head) <$> getDirectoryContents "help"
>        cs <- mapM (stringOfFile . ("help/"++)) fs
>        let mfl = mkName "M.fromList"
>        return . AppE (VarE mfl) . ListE $ zipWith (\f c -> TupE [LitE (StringL f), c]) fs cs
