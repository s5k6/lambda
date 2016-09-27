synopsis: Some very generic helper functions.
author: Stefan Klinger <http://stefan-klinger.de>

> module Helper ( module Helper, module Data.List ) where

> import Data.List ( intersperse, sort )
> import Data.Char ( isSpace )
> import System.IO



Read a file, enforcing encoding to be UTF-8, instead of letting
`readFile` figure this out.  This should avoid the dreaded `invalid
argument (invalid character)` error.

> readFileUtf8 :: FilePath -> IO String
> readFileUtf8 fp
>   = do h <- openFile fp ReadMode
>        hSetEncoding h utf8
>        hGetContents h



> compose :: Foldable t => t (b -> b) -> b -> b
> compose = foldr (.) id


> colored :: String -> ShowS -> ShowS
> colored c s
>     = showString "\001\027["
>       .
>       showString c
>       .
>       showString "m\002"
>       .
>       s
>       .
>       showString "\001\027[m\002"



> infixl 1 ?
> (?) :: Bool -> t -> t -> t
> (?) c t e = if c then t else e

> when :: Monad m => Bool -> m () -> m ()
> when c b = c ? b $ return ()

> unliness :: [ShowS] -> ShowS
> unliness = compose . intersperse (showChar '\n')        

> unwordss :: [ShowS] -> ShowS
> unwordss = compose . intersperse (showChar ' ')        

> trim :: String -> String
> trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
