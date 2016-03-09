synopsis: Data structures shared between different modules.
author: Stefan Klinger <http://stefan-klinger.de>

> module Data ( Expr(..), Command(..), Format(..), free, prettyTex, prettyUtf8, prettyPlain, primitives
>             )
>     where



> import Helper
> import qualified Data.Set as S
> import qualified Data.Map as M


--------------------------------------------------------------------------------


> data Expr
>   = Var String -- Variable
>   | Int Integer
>   | Bln Bool
>   | Str String
>   | Sym String
>   | App Expr Expr -- Funktionsapplikation
>   | Lam Bool String Expr -- λ-Abstraktion
>   | Prim String [Expr] Int
>     -- | Let Expr (M.Map String Expr) -- lokale Definition
>   deriving (Eq, Show)

> data Command
>   = Eval Expr
>   | Quit
>   | Help [String]
>   | Load [FilePath]
>   | Write (Maybe FilePath)
>   | Clear
>   | List
>   | Def String (Maybe Expr)
>   | ShowSettings
>   | Limit (Maybe Int)
>   | Trace Bool
>   | Format Format
>   deriving (Eq, Show)

> data Format = Internal | Unicode | Latex deriving Eq
>
> instance Show Format where
>   show Internal = "internal"
>   show Unicode = "unicode"
>   show Latex = "latex"

> primitives :: M.Map String (Expr, String)
> primitives
>     = M.fromList
>       [ mk "add" 2 "addition : Int → Int → Int"
>       , mk "div" 2 "division : Int → Int → Int"
>       , mk "eq"  2 "equality"
>       , mk "gt"  2 "greater than : Int → Int → Bool"
>       , mk "if"  1 "conditional : Bool → expr → expr → expr"
>       , mk "lt"  2 "lesser than : Int → Int → Bool"
>       , mk "mod" 2 "modulo : Int → Int → Int"
>       , mk "mul" 2 "multiply : Int → Int → Int"
>       , mk "neg" 1 "negate : Int → Int"
>       , mk "sub" 2 "subtract : Int → Int → Int"
>       , mk "undefined" 0 "terminates calculation"
>       ]
>     where
>     mk n s h = (n, (Prim n [] s, h))


--------------------------------------------------------------------------------
Properties


> free :: Expr -> S.Set String
> free (Var v) = S.singleton v
> free (App e1 e2) = free e1 `S.union` free e2
> free (Lam _ v e) = S.delete v $ free e
> free _ = S.empty

 

--------------------------------------------------------------------------------
How to display λ-Expressions


> colPrim, colStrict, colString, colInt, colBln, colSym :: String

> colPrim = "2;35"
> colStrict = "1;35"
> colString = "32"
> colInt = "34"
> colBln = "1;30"
> colSym = "2;36"

> class Show a => PrettyPlain a where
>     prettyPlain :: Int -> a -> ShowS
>     prettyPlain _ = shows

> instance PrettyPlain Expr where
>     prettyPlain _ (Var v) = showString v
>     prettyPlain _ (Str s) = shows s
>     prettyPlain _ (Int i) = shows i
>     prettyPlain _ (Bln b) = shows b
>     prettyPlain _ (Sym s) = showString s
>     prettyPlain p (App e1 e2)
>         = showParen (p > 2) $ prettyPlain 2 e1 . showString " " . prettyPlain 3 e2
>     prettyPlain p (Lam s v e)
>         = showParen (p > 1) ss
>         where
>         ss = showString "λ" . var s v . body e
>         var s v = (if s then showChar '!' else id) . showString v
>         body (Lam s v e) = showString " " . var s v . body e
>         body e = showString ". " . prettyPlain 1 e
>     prettyPlain p (Prim o as _)
>         = showParen (p > 3)
>           $
>           showString o
>           .
>           (compose . map (\x-> showChar ' ' . prettyPlain 4 x) $ reverse as)

 > prettyPlain p (Let e ds)
 >     | M.null ds = showParen (p > 0) $ prettyPlain 0 e
 >     | otherwise = showString "( "
 >                   . prettyPlain 0 e
 >                   . (compose . map f $ M.toList ds) . showString " )"
 >            where
 >            f (n, b) = showString "; " . showString n
 >                       .
 >                       showString " = " . prettyPlain 0 b



> class Show a => PrettyUtf8 a where
>     prettyUtf8 :: Int -> a -> ShowS
>     prettyUtf8 _ = shows

> instance PrettyUtf8 Expr where
>     prettyUtf8 _ (Var v) = showString v
>     prettyUtf8 _ (Str s) = colored colString $ shows s
>     prettyUtf8 _ (Int i) = colored colInt $ shows i
>     prettyUtf8 _ (Bln b) = colored colBln $ shows b
>     prettyUtf8 _ (Sym s) = colored colSym $ showString s
>     prettyUtf8 p (App e1 e2)
>         = showParen (p > 2) $ prettyUtf8 2 e1 . showString " " . prettyUtf8 3 e2
>     prettyUtf8 p (Lam s v e)
>         = if s
>           then colored colStrict
>                    (showChar '(') . ss . colored colStrict (showChar ')')
>           else showParen (p > 1) ss
>         where
>         ss = showString "λ" . var s v . body e
>         var s v = (if s then showChar '!' else id) . showString v
>         body (Lam s v e) = showString " " . var s v . body e
>         body e = showString ". " . prettyUtf8 1 e
>     prettyUtf8 p (Prim o as _)
>         = showParen (p > 3)
>           $
>           colored colPrim (showString o)
>           .
>           (compose . map (\x-> showChar ' ' . prettyUtf8 4 x) $ reverse as)

 > prettyUtf8 p (Let e ds)
 >     | M.null ds = showParen (p > 0) $ prettyUtf8 0 e
 >     | otherwise = showString "( "
 >                   . prettyUtf8 0 e
 >                   . (compose . map f $ M.toList ds) . showString " )"
 >            where
 >            f (n, b) = showString "; " . showString n
 >                       .
 >                       showString " = " . prettyUtf8 0 b



> class Show a => PrettyTex a where
>     prettyTex :: Int -> a -> ShowS
>     prettyTex _ = shows

 
> instance PrettyTex Expr where
>   prettyTex _ (Var v) = showString v
>   prettyTex _ (Str s) = showString "\\textrm{" . shows s . showString "}"
>   prettyTex _ (Int i) = shows i
>   prettyTex _ (Sym s) = showString "\\textbf{" . showString s . showString "}"
>   prettyTex p (App e1 e2)
>     = showParen (p > 2) $ prettyTex 2 e1 . showString "~" . prettyTex 3 e2
>   prettyTex p (Lam s v e)
>     = showParen (p > 1) $ showString "\\lambda " . var s v . body e
>     where
>       var s v = (if s then showChar '!' else id) . showString v
>       body (Lam s v e) = showString "~" . var s v . body e
>       body e = showString ".~" . prettyTex 1 e
>   prettyTex p (Prim o as _)
>     = showParen (p > 3)
>       $
>       showString "\\texttt{" . showString o . showString "}"
>       .
>       (compose . map (\x-> showChar '~' . prettyUtf8 4 x) $ reverse as)
>   prettyTex _ e
>     = error $ "LaTeX output is not implemented yet:" ++ show e

 > prettyTex p (Let e ds)
 >     | M.null ds = showParen (p > 0) $ prettyTex 0 e
 >     | otherwise = showString "( "
 >                   . prettyTex 0 e
 >                   . (compose . map f $ M.toList ds) . showString " )"
 >            where
 >            f (n, b) = showString "; " . showString n
 >                       .
 >                       showString " = " . prettyTex 0 b

 > prettyTex p (Strict n es)
 >     = showChar '«' . shows (foldr1 (flip App) es) . showString " ?" . shows n . showChar '»'

 

================================================================================
