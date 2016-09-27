synopsis: Parser for a somewhat simplified λ-calculus based language.
author: Stefan Klinger <http://stefan-klinger.de>

> module Parser
>     ( parse, parseFromFile, entirely, deflist, expression
>     , lexeme, intLiteral, strLiteral, errorPos, sourceColumn, command
>     ) where


> import Control.Applicative
> import Text.ParserCombinators.Parsec hiding
>     (newline, space, many, optional, (<|>), parseFromFile)
> import Data
> import qualified Data.Map as M
> import Helper
 
 
--------------------------------------------------------------------------------
Combinators


> skipManyTill :: Parser b -> Parser a -> Parser ()
> skipManyTill q p = void $ manyTill q (try p)

> skipTill :: Parser a -> Parser ()
> skipTill = skipManyTill anyChar

> void :: Monad m => m a -> m ()
> void = fmap $ const ()

--------------------------------------------------------------------------------
Spaces and comments


> newline, space :: Parser ()
> newline = () <$ char '\n' <?> "newline"
> space = () <$ (oneOf " \t\n" <?> "space")

> commentKeyChar :: Parser Char
> commentKeyChar
>     = satisfy (\c -> '#' < c && c < '<'
>                   || c == '='
>                   || '>' < c && c <= '~'
>               )

> comment :: Parser ()
> comment
>     = char '#' >> (cutk =<< try (manyTill commentKeyChar $ char '>')) <|> ceol
>     where
>     ceol = skipTill (newline <|> eof)
>     cutk k = skipTill . string $ '<':k++"#"

> whitespace :: Parser ()
> whitespace
>     = skipMany (space <|> comment <?> "whitespace")

> lexeme :: Parser a -> Parser a
> lexeme p
>     = do x <- p
>          whitespace
>          return x


This version parses from file, but enforces UTF-8 encoding.

> parseFromFile :: Parser a -> FilePath -> IO (Either ParseError a)
> parseFromFile p fp
>   = do input <- readFileUtf8 fp
>        return (runParser p () fp input)


--------------------------------------------------------------------------------
Der top-level Parser kümmert sich um Leerzeichen am Anfang des Input und um EOF.


> entirely :: Parser a -> Parser a
> entirely = between whitespace eof

> deflist :: Parser (M.Map String Expr)
> deflist = M.fromList <$> definition `endBy` lexeme (char ';')

> binder :: Parser (Bool, String)
> binder = (,) <$> ((char '!' >> return True) <|> return False) <*> varname

> definition :: Parser (String, Expr)
> definition
>     = do f <- varname
>          as <- many binder
>          void $ lexeme $ char '='
>          b <- expression
>          return (f, foldr (uncurry Lam) b as)

<app> : <lexpr>+    Um mehrstellige Funktionsanwendung (Currying) darzu-
                    stellen, fassen wir eine Applikation als nicht-leere
                    Folge von Ausdrücken auf.

> app :: Parser Expr
> app = foldl1 App <$> many1 lexpr


<lexpr> ::= <var>           Ein Ausdruck ist dann in unserer Grammatik entweder
         |  <con>           eine Variable, eine Konstante, eine Abstraktion,
         |  <abst>          oder ein Klammerausdruck, evtl mit lokalen
         |  <paren>         Definitionen.
 
> lexpr :: Parser Expr
> lexpr = abst <|> varOrPrim <|> con <|> paren




<name> ::= für Primitive oder Variablen

> name :: Parser String
> name = lexeme $ (:) <$> lower <*> many (alphaNum <|> oneOf "_'")

> varOrPrim :: Parser Expr
> varOrPrim = do n <- name
>                return . maybe (Var n) fst $ M.lookup n primitives

<var> ::= eine nicht-leere Folge von Buchstaben

> var :: Parser Expr
> var = do q <- varOrPrim
>          case q of
>            Var _  -> return q
>            Prim p _ _ -> fail $ "Found primitive `"++p++"`, expected variable"
>            _ -> error "Unreachable '6qdPGh4Sgewj'"

> varname :: Parser String
> varname = do Var v <- var
>              return v


<op> ::= eine nicht-leere Folge von Interpunktionen

 > op :: Parser Expr
 > op = Var <$> opname

 > opname = lexeme $ (:) <$> letter <*> many (oneOf "~!@%^&*/-+=")
 

<con> ::= <string> | <integer> | `__`

> con :: Parser Expr
> con = choice [ Str <$> strLiteral
>              , Int <$> intLiteral
>              , Bln <$> blnLiteral
>              , Sym <$> symLiteral
>              ]
 
> intLiteral :: Parser Integer
> intLiteral
>     = lexeme $ f . map (toInteger . subtract o . fromEnum) <$> many1 digit
>     where
>     o = fromEnum '0'
>     f = foldl (\n d -> 10 * n + d) 0

> blnLiteral :: Parser Bool
> blnLiteral
>     = lexeme . try
>       $
>       do b <- choice [ string "True" >> return True
>                      , string "False" >> return False
>                      ]
>          notFollowedBy alphaNum
>          return b


> strLiteral :: Parser String
> strLiteral
>   = lexeme $ between (char '"') (char '"') $ concat <$> many character
>   where
>   character = escapedChar <|> (return <$> noneOf "\\\"")

> escapedChar :: Parser String
> escapedChar
>   = char '\\'
>     >>
>     choice
>     [ char '\\' >> return "\\"
>     , char '"' >> return "\""
>     , char 'n' >> return "\n"
>     , char 't' >> return "\t"
>     , char ' ' >> return " "
>     , char '\n' >> many space >> char '\\' >> return ""
>     ]

> symLiteral :: Parser String
> symLiteral
>     = lexeme $ (:) <$> upper <*> many (alphaNum <|> oneOf "_'")
 

<abst> ::= `\` (`!`? <var>)+ `.` <expression>

> abst :: Parser Expr
> abst = do void . lexeme $ (char '\\' <|> char 'λ')
>           ps <- many1 binder
>           void . lexeme $ string "."
>           b <- expression
>           return $ foldr (uncurry Lam) b ps


<expression> : <expression> `$` <app>
             | <app>

> expression :: Parser Expr
> expression = foldr1 App <$> (sepBy1 app . lexeme $ char '$')



<paren> : `(` <app> (`;` <var> `=` <expression>)* `)`

Klammerausdrücke können mit lokalen Bindungen erweitert werden:
`(f 3; f x = x * x)` entspricht `let f x = x * x in f 3`

> paren :: Parser Expr
> paren = between (lexeme $ char '(') (lexeme $ char ')')
>         $
>         do e <- expression
>            ds <- many (lexeme (char ';') >> definition)
>            if null ds
>            then return e
>            else fail "Sorry, let-expressions (of the form `expr; defs`) are\
>                      \ not yet implemented."
>                 -- return . Let e $ M.fromList ds


--------------------------------------------------------------------------------
Parser für Eingaben am interaktiven Prompt


> key :: String -> Parser ()
> key k = try . lexeme $ string k >> notFollowedBy alphaNum

> word :: Parser String
> word = strLiteral <|> naked
>     where
>     naked = lexeme $ concat <$> many1 character
>     character = escapedChar <|> (return <$> noneOf "\\\" ")


> command :: Parser Command
> command
>   = entirely
>     $
>     ( char ':'
>       >>
>       choice
>       [ key "h" >> Help <$> many word
>       , key "quit" >> return Quit
>       , key "l" >> Load <$> many word
>       , key "w" >> Write <$> optional word
>       , key "r" >> fail "Command `:r` is deprecated, use `:l` with no arg\
>                         \uments instead."
>       , key "c" >> fail "Command `:c` is deprecated, use `:d *=` instead."
>       , key "d" >> choice [ try $ (\(s,e) -> Def s $ Just e) <$> definition
>                           , Def <$> varname <*> pure Nothing <* key "="
>                           , key "*" >> key "=" >> return Clear
>                           , return List
>                           ]
>       , key "set" >> option ShowSettings setting
>       ]
>     )
>     <|>
>     Eval <$> expression

> setting :: Parser Command
> setting
>   = choice
>     [ key "limit" >> Limit <$> ((Just . fromIntegral
>                                  <$> intLiteral) <|> (Nothing <$ key "none"))
>     , key "trace" >> Trace <$> (True <$ key "all" <|> False <$ key "none")
>     , key "format" >> Format <$> choice [ Internal <$ key "internal"
>                                         , Unicode  <$ key "unicode"
>                                         , Latex    <$ key "latex"
>                                         ]
>     ]
