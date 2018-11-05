module CandleLight.Parser where

import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellStyle)
import Data.Functor.Identity

import CandleLight.Lang.Expr

allParser :: Parsec String u [Expr]
allParser = do
    x <- fmap (const nil) eof <|> exprParser
    xs <- fmap (const []) eof <|> (skipMany space >> allParser)
    pure (x : xs)

exprParser :: Parsec String u Expr
exprParser  = (ExprList <$> listParser)
           <|> (Lit <$> litParser)
           <|> (Atom <$> try (char '.' >> identifier))
           <|> (Ident <$> identifier)

strParser :: Parsec String u Expr
strParser = fmap (ExprList . fmap (Lit . LitChar)) stringLiteral

listParser :: Parsec String u [Expr]
listParser = emptyParse <|> (skipMany space >> char '(' >> subParser)
    where
        emptyParse = try $ const [] <$> string "()"
        subParser = do
            x <- exprParser
            xs <- try (skipMany space >> const [] <$> char ')') <|> (skipMany space >> subParser)
            pure $ x : xs

litParser :: Parsec String u Literal
litParser = try (LitFloat <$> float) 
         <|> (LitInt <$> natural)
         <|> try (LitInt . (0-) <$> (char '-' >> natural))
         <|> (LitChar <$> charLiteral)



lexer :: P.GenTokenParser String u Identity
lexer       = P.makeTokenParser $ haskellStyle {
    P.identStart = letter <|> P.opLetter haskellStyle,
    P.identLetter = P.identLetter haskellStyle <|> P.opLetter haskellStyle
}

parens :: Parsec String u a -> Parsec String u a
parens      = P.parens lexer

braces :: Parsec String u a -> Parsec String u a
braces      = P.braces lexer

identifier :: Parsec String u String
identifier  = P.identifier lexer

reserved :: String -> Parsec String u ()
reserved    = P.reserved lexer

natural :: Parsec String u Integer
natural = P.natural lexer

integer :: Parsec String u Integer
integer = P.integer lexer

float :: Parsec String u Double
float = P.float lexer

charLiteral :: Parsec String u Char
charLiteral = P.charLiteral lexer

stringLiteral :: Parsec String u String
stringLiteral = P.stringLiteral lexer