> module Helper ( module Helper, module Data.List ) where

> import Data.List ( intersperse, sort )



> compose :: Foldable t => t (b -> b) -> b -> b
> compose = foldr (.) id


> colored :: String -> (ShowS) -> ShowS
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



> infixl 8 ?
> (?) :: Bool -> t -> t -> t
> (?) c t e = if c then t else e

> when :: Monad m => Bool -> m () -> m ()
> when c b = c ? b $ return ()

> unliness :: [ShowS] -> ShowS
> unliness = compose . intersperse (showChar '\n')        
