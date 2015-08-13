> module Simple where

> import qualified Data.Set as S
> import qualified Data.Map as M
> import qualified System.Environment as E
> import System.Console.Haskeline as H
> import Control.Monad.Trans ( lift )
> import Control.Monad
> import Parser
> import Data
> import Helper
> import qualified CompileTime as C



If the fallible function `f` finds a new value, then `try f` returns
that.  Otherwise, it returns the original.

> try :: (b -> Maybe b) -> b -> b
> try f e = maybe e id $ f e


This substitution avoids the excessive introduction of new `App`
nodes, if the subexpressions have not changed.

> subst :: Expr -> String -> Expr -> Expr
> subst m x = try go
>     where
>     go (Var v) | x==v = Just m
>     go (App e1 e2)
>         = maybe
>           (App e1 <$> go e2)
>           (\e1 -> Just . App e1 $ try go e2)
>           (go e1)
>     go (Lam s v e)
>         | x==v          = Nothing
>         | S.member v fm = go $ alpha fm (Lam s v e)
>         | otherwise     = Lam s v <$> go e
>     go _ = Nothing
>     fm = free m

…and the same is being done for α-conversions.  They sould really be
refactored.

> alpha :: S.Set String -> Expr -> Expr
> alpha free = try go
>     where
>     go (App e1 e2)
>       = maybe
>         (App e1 <$> go e2)
>         (\e1 -> Just . App e1 $ try go e2)
>         (go e1)
>     go (Lam s x b)
>         | S.member x free
>             = let x' = head [ z
>                             | n <- [0::Int ..]
>                             , let z = x++show n
>                             , not $ S.member z free
>                             ]
>                   b' = subst (Var x') x b
>               in Just . Lam s x' $ try go b'
>         | otherwise = Lam s x <$> go b
>     go _ = Nothing


> data Reason
>     = Beta
>     | Delta
>     | Lookup
>     | Other String
>     deriving (Eq, Ord)

> instance Show Reason where
>     showsPrec _ Beta = showString "β→"
>     showsPrec _ Delta = showString "δ→"
>     showsPrec _ Lookup = showString " ="
>     showsPrec _ (Other t) = showString t


> data Steps
>     = Step Reason Expr Steps
>     | Finished
>     | Failed String

> type Stack = [(Expr,[Expr])]

> whnf :: M.Map String Expr -> Expr -> Steps
> whnf st = \e-> go e [] []
>     where
>
>     go :: Expr -> [Expr] -> Stack -> Steps
>
>     go (App e1 e2) es = go e1 (e2:es)
>
>     go l@(Lam s v b) (e:es)
>       = if s
>         then go e [] . ((l, es):)
>         else step Beta (subst e v b) es
>
>     go (Var v) es
>       = case M.lookup v st of
>           Nothing -> const (Failed $ "Variable `"++v++"` unknown.")
>           Just e' -> step Lookup e' es
>
>     go (Prim q as 0) es = delta q (reverse as) es
>     go p@(Prim _ _ n) (e:es) | n <= 1+length es = go e [] . ((p,es) :)
>
>     go e es = back e es -- so here `e:es` is in whnf

>     step :: Reason -> Expr -> [Expr] -> Stack -> Steps
>     step r e es ss
>       = Step
>         r
>         (foldl (\e (s, es) -> foldl App s (e:es)) (foldl App e es) ss)
>         (go e es ss)

>     back :: Expr -> [Expr] -> Stack -> Steps
>     back e es ((x, xs):ss)
>         = case x of
>             Lam _ v b   -> step Beta (subst e' v b) xs ss
>             Prim p as n -> go (Prim p (e':as) (n-1)) xs ss
>             _other      -> error "never happens?" -- go (foldl App e' xs) ss
>         where
>         e' = foldl App e es

>     back _ _ [] = Finished -- empty stack

>     delta :: String -> [Expr] -> [Expr] -> Stack -> Steps
>     delta "add" [Int a, Int b] es = step Delta (Int $ a + b) es
>     delta "div" [Int a, Int b] es = step Delta (Int $ a `div` b) es
>     delta "eq"  [a, b] es = step Delta (Bln $ a==b) es
>     delta "gt"  [Int a, Int b] es = step Delta (Bln $ a > b) es
>     delta "if"  [Bln b] (t:f:es) = step Delta (if b then t else f) es
>     delta "lt"  [Int a, Int b] es = step Delta (Bln $ a < b) es
>     delta "mod" [Int a, Int b] es = step Delta (Int $ a `mod` b) es
>     delta "mul" [Int a, Int b] es = step Delta (Int $ a * b) es
>     delta "neg" [Int a] es = step Delta (Int $ negate a) es
>     delta "sub" [Int a, Int b] es = step Delta (Int $ a - b) es
>     delta "undefined" _ _ = const $ Failed "reached ⊥"
>     delta _ _ _ = const $ Failed "Cannot δ-reduce this"


================================================================================

WAIT — all code below this line is a stinking pile of crap!


> type Bindings = M.Map String Expr

> data Status
>     = Status { good :: Bool
>              , env :: M.Map String Expr
>              , limit :: Maybe Int
>              , trace :: Bool
>              , lastLoad :: [String]
>              , format :: Format
>              }

> main :: IO ()
> main = do putStrLn $ colored "1;30" (showString "Primitive λ-evaluator") . showString " — Type `:h` for help.  " . colored "1;31" (showString "☠ experimental ☣") $ ""
>           as <- E.getArgs
>           hist <- (++) <$> E.getEnv "HOME" <*> pure "/.lambda/history"
>           st <- if null as then return st else cmdLoad as st
>           let unescapable = catchCtrlC unescapable $ repl Nothing st
>           H.runInputT H.defaultSettings{ historyFile = Just hist }
>             (withInterrupt unescapable)
>     where
>     st = Status { good = True
>                 , env = M.empty
>                 , limit = Just 1000
>                 , trace = True
>                 , lastLoad = []
>                 , format = Unicode
>                 }

> type Repl = Status -> H.InputT IO ()

> catchCtrlC :: MonadException m => InputT m a -> InputT m a -> InputT m a
> catchCtrlC fallback what = handle handler what
>     where
>     handler Interrupt
>       = do outputStrLn $ colored "31" (showString "[Ctrl-C]") ""
>            fallback


One may feed `repl` with an initial input, and a cursor position.

> repl :: Maybe (String,Int) -> Repl
> repl retry st
>   = do l <- maybe
>             (H.getInputLine p)
>             (\(str,n) -> H.getInputLineWithInitial p $ splitAt n str)
>             retry
>        case l of
>         Nothing -> outputStrLn "\nbye"
>         Just [] -> repl Nothing st
>         Just text
>           -> case parse command "your input" text of
>               Left msg
>                 -> do outputStrLn $ show msg
>                       repl (Just (text, pred . sourceColumn . errorPos $ msg))
>                            st{ good = False }
>               Right cmd
>                 -> case cmd of
>                     Quit
>                       -> outputStrLn "\nbye"
>                     Eval expr
>                       -> do g <- lift
>                                  .
>                                  report (limit st) (trace st)
>                                  (printer $ format st)
>                                  .
>                                  Step (Other "") expr
>                                  $
>                                  whnf (env st) expr
>                             repl Nothing st{ good = g }
>                     Def v (Just e)
>                       -> repl Nothing st{ env = M.insert v e $ env st, good = True }
>                     Def v Nothing
>                       -> repl Nothing st{ env = M.delete v $ env st, good = True }
>                     Clear
>                       -> repl Nothing st{ env = M.empty, good = True }
>                     Load []
>                       -> lift (cmdLoad (lastLoad st) st) >>= repl Nothing
>                     Load xs
>                       -> lift (cmdLoad xs st) >>= repl Nothing
>                     List
>                       -> lift (cmdList st) >>= repl Nothing
>                     Help ts
>                       -> lift (cmdHelp ts) >> repl Nothing st{ good = True }
>                     ShowSettings
>                       -> do outputStr $ unlines
>                               [ "limit " ++ maybe "none" show (limit st)
>                               , "trace " ++ (trace st ? "all" $ "none")
>                               , "format " ++ show (format st)
>                               ]
>                             repl Nothing st{ good = True }
>                     Limit l
>                       -> repl Nothing st{ limit = l, good = True }
>                     Trace t
>                       -> repl Nothing st{ trace = t, good = True }
>                     Format f
>                       -> repl Nothing st{ format = f, good = True }
>   where
>   p = colored (good st ? "1;32" $ "1;31") (showString "λ> ") ""


> printer :: Format -> Expr -> ShowS
> printer Internal = shows
> printer Unicode = prettyUtf8 0
> printer Latex = prettyTex 0



> data Stats = Stats { stTotal :: !Int
>                    , stBeta :: !Int
>                    , stDelta :: !Int
>                    , stLookup :: !Int
>                    }
>
> instance Show Stats where
>     showsPrec _ (Stats t b d l)
>         = shows t . showString " steps (" . c . showChar ')'
>         where
>         c = compose
>             $
>             intersperse
>             (showString " + ")
>             [ shows n . showString t
>             | (n,t) <- [ (b," β-reductions")
>                        , (d," δ-reductions")
>                        , (l," lookups")
>                        ]
>             , n > 0
>             ]


> report :: Maybe Int       -- step count limit
>        -> Bool            -- tracing
>        -> (Expr -> ShowS) -- printer
>        -> Steps           -- from `whnf`
>        -> IO Bool         -- success
> report lim tr pr = go (Stats 0 0 0 0) (Stats 0 0 0 0) Nothing
>     where
>     go stats _ _ _ | maybe False (stTotal stats >=) lim
>       = do putStrLn $ colored "31"
>              ( showString "LIMIT EXCEEDED ("
>              . shows stats
>              . showString ")"
>              ) ""
>            return False
>     go stats ostats last Finished
>       = do mbPrintLast last stats ostats
>            putStrLn $ colored "2;37" (shows stats) ""
>            return True
>     go stats ostats last (Failed msg)
>       = do mbPrintLast last stats ostats
>            putStrLn $ colored "31" (showString msg) ""
>            putStrLn $ colored "2;37" (shows stats) ""
>            return False
>     go stats ostats last (Step r e ss)
>         | tr  -- S.member r tr
>             = do mbPrintLast last stats ostats
>                  putStrLn $ colored "2;36" (shows r) ""
>                  putStrLn $ showString "   " . pr e $ ""
>                  go stats' stats' Nothing ss
>         | otherwise
>             = go stats' ostats (Just e) ss
>       where
>       stats'
>         = case r of
>             Beta -> stats{ stTotal = stTotal stats + 1
>                          , stBeta = stBeta stats + 1
>                          }
>             Delta -> stats{ stTotal = stTotal stats + 1
>                           , stDelta = stDelta stats + 1
>                           }
>             Lookup -> stats{ stTotal = stTotal stats + 1
>                            , stLookup = stLookup stats + 1
>                            }
>             _ -> stats
>     mbPrintLast (Just l) ns os
>         = do putStrLn . colored "2;36" (showString "*→   ") . colored
>                                     "2;37" (shows $ statsDiff ns os) $ ""
>              putStrLn $ showString "   " . pr l $ ""
>         where
>         statsDiff (Stats nt nb nd nl) (Stats ot ob od ol)
>             = Stats (nt-ot) (nb-ob) (nd-od) (nl-ol)
>     mbPrintLast _ _ _ = return ()



> cmdList :: Status -> IO Status
> cmdList st
>     = do mapM_ (\(v,e) -> putStrLn . showString v
>                           . showString " = " . (printer $ format st) e $ ";")
>            . M.toList $ env st
>          putStrLn $ "Total of " ++ show (M.size $ env st)
>            ++ " definitions."
>          return st{good = True}

> cmdLoad :: [String] -> Status -> IO Status
> cmdLoad fs st
>     = do st <- foldM load st{ env = M.empty, lastLoad = fs } fs
>          putStrLn $ "Total of " ++ show (M.size $ env st)
>                           ++ " definitions."
>          return st
>     where
>     load st fp
>       = do ds <- parseFromFile (entirely deflist) fp
>            case ds of
>             Left msg
>               -> do putStrLn (show msg)
>                     return st{ good = False }
>             Right ds
>               -> do putStrLn $ "Loaded " ++ show (M.size ds)
>                       ++ " definitions from `" ++ fp ++ "`."
>                     return st{ env = M.union ds $ env st }



> cmdHelp :: [String] -> IO ()
> cmdHelp []
>   = cmdHelp ["basic"]
> cmdHelp ts = putStrLn "" >> mapM_ help ts
>   where
>     help "list"
>       = do putStrLn "The following help topics ara available:\n"
>            putStrLn . unlines . map ("    :h "++)
>              . S.toList . S.insert "version" . S.insert "list" . S.insert "primitives"
>              $ M.keysSet C.help
>     help "version"
>       = do putStrLn
>              $ unlines
>              [ "Revision: " ++ C.revision
>              , "Compiled: " ++ C.date
>              ]
>     help "primitives"
>       = do putStrLn "Primitives with their number of arguments required \
>                     \in WHNF:\n"
>            mapM_ f $ M.toList primitives
>            putStrLn ""
>       where
>         f (n, (Prim _ _ s, h))
>          = putStrLn $ "    " ++ n ++ " — " ++ show s ++ ", " ++ h
>         f _ = error "There should be only primitives!"
>     help t
>       = case M.lookup t C.help of
>           Just h -> putStrLn h
>           Nothing -> putStrLn $ "There is no help topic `"++t++"`."
