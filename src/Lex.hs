module Lex (Token (..), tokens) where

import Data.Char
import Value
import Pos
import Result
import Print
import Operator

data Token = Paren [Tag Token]
           | Curly [Tag Token]
           | Square [Tag Token]
           | Symbol String
           | Value WrappedValue
           | Semicolon
           | Operator Operator
           deriving (Eq)

instance Show Token where
  show (Paren xx) = '(' : showListNoBrace xx ++ ")"
  show (Curly xx) = '{' : showListNoBrace xx ++ "}"
  show (Square xx) = '[' : showListNoBrace xx ++ "]"
  show (Symbol s) = s
  show (Value v) = show v
  show Semicolon = ";"
  show (Operator op) = show op

flipBrace :: Char -> Char
flipBrace '(' = ')'
flipBrace ')' = '('
flipBrace '{' = '}'
flipBrace '}' = '{'
flipBrace '[' = ']'
flipBrace ']' = '['
flipBrace _ = error "Not a brace"

isOpen, isClose :: Char -> Bool
isOpen = flip elem ['(', '{', '[']
isClose = flip elem [')', '}', ']']

isAlphaU, isAlphaNumU :: Char -> Bool
isAlphaU x = x == '_' || isAlpha x
isAlphaNumU x = x == '_' || isAlphaNum x

tag :: Pos -> [Char] -> [Tag Char]
tag _ [] = []
tag pos (x:xs) = (Tag pos x):tag (advance pos x) xs

{- |
>>> Lex.tokens "*stdin*" "(a b { c; }) [x] 13"
[(a, b, {c, ;}),[x],13]
>>> Lex.tokens "*stdin*" "{)"
*stdin*:1:2: Found ) when expected } to close { at *stdin*:1:1.
>>> Lex.tokens "*stdin*" "a, b"
[a,,,b]
-}
tokens :: String -> String -> Result [Tag Token]
tokens fileName = fmap fst . parse Nothing . tag (Pos fileName 1 1)

parseRest :: Maybe (Tag Char) -> [Tag Char] -> Tag Token -> Result ([Tag Token], [Tag Char])
parseRest t xs token = do
  (tokens, rest) <- parse t xs
  return (token:tokens, rest)

parseString :: [Tag Char] -> Result ([Tag Char], String)
parseString [] = fail "EOF while parsing a string"
parseString (Tag _ '"':xs) = return (xs, "")
parseString xx = do
  (xs, c) <- parseChar xx
  (xs', s) <- parseString xs
  return (xs', c:s)

parseChar :: [Tag Char] -> Result ([Tag Char], Char)
parseChar (Tag _ '\\':Tag _ '\\':xs) = do
  return (xs, '\\')
parseChar (Tag _ '\\':Tag _ '"':xs) = do
  return (xs, '"')
parseChar (Tag _ '\\':Tag _ 'n':xs) = do
  return (xs, '\n')
parseChar (Tag _ '\\':Tag _ 't':xs) = do
  return (xs, '\t')
parseChar (Tag _ x:xs) = do
  return (xs, x)
parseChar [] = fail "EOF while parsing a char"

parse :: Maybe (Tag Char) -> [Tag Char] -> Result ([Tag Token], [Tag Char])
parse Nothing [] = return ([], [])
parse (Just (Tag p t)) [] = fail $ show p ++ ": Reached end of file before closing " ++ show t ++ " was reached."
parse t (Tag p ';':xs) = parseRest t xs . Tag p $ Semicolon
parse t (Tag p ',':xs) = parseRest t xs . Tag p $ Operator Comma
parse t (Tag p '+':xs) = parseRest t xs . Tag p $ Operator Plus
parse t (Tag p '-':xs) = parseRest t xs . Tag p $ Operator Minus
parse t (Tag p '*':xs) = parseRest t xs . Tag p $ Operator Times
parse t (Tag p '/':xs) = parseRest t xs . Tag p $ Operator Divide
parse t (Tag p '=':xs) = parseRest t xs . Tag p $ Operator Assign
parse _ (Tag p '\'':Tag _ '\'':_) = fail $ show p ++ ": Cannot have empty char."
parse t (Tag p '\'':xs) = do
  (xs', c) <- parseChar xs
  parseRest t xs' . Tag p . Value $ WVChar c
parse t (Tag p '"':xs) = do
  (xs', str) <- parseString xs
  parseRest t xs' . Tag p . Value $ WVString str
parse t (Tag p x:xs) | isOpen x = do
                         (inside, rest) <- parse (Just . Tag p $ flipBrace x) xs
                         (outside, rest) <- parse t rest
                         return (Tag p (open x inside):outside, rest)
                           where open '(' = Paren
                                 open '{' = Curly
                                 open '[' = Square
parse (Just (Tag pt t)) (Tag p x:xs) | t == x = return ([], xs)
                                     | isClose x = fail (show p ++ ": Found " ++ x : " when expected " ++
                                                         t : " to close " ++ flipBrace t : " at " ++ show pt ++ ".")
parse Nothing (Tag p x:xs) | isClose x = fail (show p ++ ": Unexpected " ++ show x ++ " here, no matching " ++
                                               show (Lex.flipBrace x) ++ " present.")
parse t (Tag _ x:xs) | isSpace x = parse t xs
parse t (Tag p x:xs) | isAlphaU x = do
                         (tokens, rest2) <- parse t rest
                         return (s':tokens, rest2)
  where (s, rest) = span (isAlphaNumU . unwrapTag) xs
        s' = Tag p . Symbol $ x:map unwrapTag s
parse t (Tag p x:xs) | isNumber x = do
                         (tokens, rest2) <- parse t rest
                         return (n':tokens, rest2)
  where (n, rest) = span (isNumber . unwrapTag) xs
        n' = Tag p . Value . WVInt . read $ x:map unwrapTag n
parse _ (Tag p x:_) = error $ show p ++ ": Invalid character to lex: " ++ [x]
