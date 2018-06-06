module Parse where

import qualified Lex
import Pos
import Value
import Result
import Control.Monad
import Print
import Data.List (intercalate)
import Operator

data Expression type' = Var String
                      | Value WrappedValue
                      | FunctionCall (Tag (Expression type')) [Tag (Expression type')]
                      | Index (Tag (Expression type')) (Tag (Expression type'))
                      | Paren (Tag (Expression type'))
                      | TypeCast type' (Tag (Expression type'))
                      | InfixOperator Operator (Tag (Expression type')) (Tag (Expression type'))

data TypePlaceholder = TPSymbol String
                     | TPPointer TypePlaceholder

data Statement type' = Braced [Tag (Statement type')]
                     | Expr (Expression type')
                     | VarDecl type' String (Maybe (Tag (Expression type')))
                     | EmptyStatement

instance Show type' => Show (Expression type') where
  show (Var s) = s
  show (Value v) = show v
  show (FunctionCall (Tag _ f) x) = showMaybeParen f ++ "(" ++ showListNoBrace x ++ ")"
  show (Index (Tag _ a) i) = showMaybeParen a ++ "[" ++ show i ++ "]"
  show (Paren x) = '(' : show x ++ ")"
  show (TypeCast type' (Tag _ value)) = "(" ++ show type' ++ ") " ++ showMaybeParen value
  show (InfixOperator op (Tag _ l) (Tag _ r)) = showMaybeParen l ++ " " ++ show op ++ " " ++ showMaybeParen r

showMaybeParen :: Show type' => Expression type' -> String
showMaybeParen e@(InfixOperator _ _ _) = "(" ++ show e ++ ")"
showMaybeParen e@(TypeCast _ _) = "(" ++ show e ++ ")"
showMaybeParen x = show x

instance Show TypePlaceholder where
  show (TPSymbol s) = s

instance Show type' => Show (Statement type') where
  show (Braced []) = "{}"
  show (Braced b) = "{ " ++ intercalate " " (map show b) ++ " }"
  show (Expr e) = show e ++ ";"
  show (VarDecl t n v) = show t ++ " " ++ n ++ (case v of Just (Tag _ v') -> " = " ++ showMaybeParen v'; Nothing -> "") ++ ";"
  show EmptyStatement = "Ω"

argumentize :: [Tag (Expression type')] -> Tag (Expression type') -> [Tag (Expression type')]
argumentize xs (Tag _ (InfixOperator Comma l r)) = argumentize (r:xs) l
argumentize xs x = (x:xs)

parseExpressionParenthesis :: [Tag Lex.Token] -> Result (Tag (Expression type'))
parseExpressionParenthesis inside = do
  (inside', rest) <- parseExpression inside
  unless (null rest) . fail $ (show . pos $ head rest) ++ ": There can only be a single expression in parenthesis."
  return inside'

parseSuffixes :: Precedence -> Tag (Expression type') -> [Tag Lex.Token] -> Result (Tag (Expression type'), [Tag Lex.Token])
parseSuffixes prec f (Tag p (Lex.Paren paren):xs) = do
  paren' <- parseExpressionParenthesis paren
  parseSuffixes prec (Tag p (FunctionCall f (argumentize [] paren'))) xs
parseSuffixes prec f (Tag p (Lex.Square square):xs) = do
  square' <- parseExpressionParenthesis square
  parseSuffixes prec (Tag p (Index f square')) xs
parseSuffixes prec f (Tag p (Lex.Operator op):xs) | continue prec (precedence op) = do
  (rightSide, xs') <- parseExpression' (precedence op) xs
  parseSuffixes prec (Tag p (InfixOperator op f rightSide)) xs'
--parseSuffixes prec f xx@(Tag p t:_) | isOperator t = Ok (f, xx)
parseSuffixes _ f xx = return (f, xx)

{- |
>>> parseExpression =<< Lex.tokens "*stdin*" "(a)"
((a),[])
>>> parseExpression =<< Lex.tokens "*stdin*" "(a)(b)"
((a)(b),[])
>>> parseExpression =<< Lex.tokens "*stdin*" "a(b)"
(a(b),[])
>>> parseExpression =<< Lex.tokens "*stdin*" ""
EOF reached while parsing expression.
>>> parseExpression =<< Lex.tokens "*stdin*" "a(b)[c]"
(a(b)[c],[])
>>> parseExpression =<< Lex.tokens "*stdin*" "a, b"
(a , b,[])
>>> parseExpression =<< Lex.tokens "*stdin*" "a, b, c"
((a , b) , c,[])
>>> parseExpression =<< Lex.tokens "*stdin*" "a(b)[c], b(a, c), c[d]"
((a(b)[c] , b(a, c)) , c[d],[])
>>> parseExpression =<< Lex.tokens "*stdin*" "a + b , c + d"
((a + b) , (c + d),[])
-}
parseExpression :: [Tag Lex.Token] -> Result (Tag (Expression type'), [Tag Lex.Token])
parseExpression = parseExpression' nullPrecedence

parseExpression' :: Precedence -> [Tag Lex.Token] -> Result (Tag (Expression type'), [Tag Lex.Token])
parseExpression' prec (Tag p (Lex.Paren x):xs) = do
  x' <- parseExpressionParenthesis x
  parseSuffixes prec (Tag p $ Paren x') xs
parseExpression' prec (Tag p (Lex.Symbol s):xs) = parseSuffixes prec (Tag p $ Var s) xs
parseExpression' prec (Tag p (Lex.Value v):xs) = parseSuffixes prec (Tag p $ Value v) xs
parseExpression' _ (Tag p (Lex.Curly _):_) = fail $ show p ++ ": Cannot have curly braces inside an expression."
parseExpression' _ (Tag p Lex.Semicolon:_) = fail $ show p ++ ": Cannot have a semicolon before an expression."
parseExpression' _ (Tag p (Lex.Operator _):_) = fail $ show p ++ ": Cannot have an operator before an expression."
parseExpression' _ (Tag p (Lex.Square _):_) = fail $ show p ++ ": Cannot have square braces before an expression."
parseExpression' _ [] = fail $ "EOF reached while parsing expression."

{- |
>>> parseStatements =<< Lex.tokens "*stdin*" "a(b);;"
[a(b);]
>>> parseStatements =<< Lex.tokens "*stdin*" "{a(b);}"
[{ a(b); }]
>>> parseStatements =<< Lex.tokens "*stdin*" "{a(b); y(x);}"
[{ a(b); y(x); }]
>>> parseStatements =<< Lex.tokens "*stdin*" "{a(b); y(x);} {}"
[{ a(b); y(x); },{}]
>>> parseStatements =<< Lex.tokens "*stdin*" "[123];"
*stdin*:1:1: Cannot have square braces before an expression.
>>> parseStatements =<< Lex.tokens "*stdin*" "123 {123;}"
*stdin*:1:5: Expected a semicolon here to finish the previous expression.
>>> parseStatements =<< Lex.tokens "*stdin*" "{a(b); x}"
*stdin*:1:8: EOF reached before semicolon to finish statement here.
>>> parseStatements =<< Lex.tokens "*stdin*" "int a;"
[int a;]
>>> parseStatements =<< Lex.tokens "*stdin*" "a x; b y;"
[a x;,b y;]
>>> parseStatements =<< Lex.tokens "*stdin*" "int a = 3;"
[int a = 3;]
>>> parseStatements =<< Lex.tokens "*stdin*" "int x = 3 + 2; x = x + 1;"
[int x = (3 + 2);,x = (x + 1);]
-}
parseStatements :: [Tag Lex.Token] -> Result [Tag (Statement TypePlaceholder)]
parseStatements [] = return []
parseStatements xx = do
  (stmt@(Tag _ stmt'), xs) <- parseStatement xx
  stmts <- parseStatements xs
  case stmt' of
    EmptyStatement -> return stmts
    _ -> return (stmt:stmts)

parsePointers :: [Tag Lex.Token] -> Result ([Tag Lex.Token], TypePlaceholder -> TypePlaceholder)
parsePointers xx@(Tag _ (Lex.Symbol _):_) = return (xx, id)
parsePointers (Tag _ (Lex.Operator Times):xs) = fmap (fmap $ fmap TPPointer) $ parsePointers xs
parsePointers [] = fail ""

parseStatement :: [Tag Lex.Token] -> Result (Tag (Statement TypePlaceholder), [Tag Lex.Token])
parseStatement (Tag p (Lex.Curly x):xs) = do
  stmts <- parseStatements x
  return (Tag p (Braced stmts), xs)
parseStatement (Tag p Lex.Semicolon:xs) = return (Tag p EmptyStatement, xs)
parseStatement (Tag p (Lex.Symbol t):Tag _ (Lex.Symbol n):Tag _ Lex.Semicolon:xs) = do
  return (Tag p (VarDecl (TPSymbol t) n Nothing), xs)
parseStatement (Tag p (Lex.Symbol t):Tag _ (Lex.Operator Times):Tag _ (Lex.Symbol n):Tag _ (Lex.Operator Assign):xs) = do
  (exp, xs') <- parseExpression xs
  case xs' of
    (Tag _ Lex.Semicolon:xs'') -> return (Tag p (VarDecl (TPPointer $ TPSymbol t) n (Just exp)), xs'')
    (Tag p _:_) -> fail $ show p ++ ": Expected semicolon here."
    [] -> fail $ show (pos exp) ++ ": EOF reached before semicolon to finish statement here."
parseStatement (Tag p (Lex.Symbol t):Tag _ (Lex.Symbol n):Tag _ (Lex.Operator Assign):xs) = do
  (exp, xs') <- parseExpression xs
  case xs' of
    (Tag _ Lex.Semicolon:xs'') -> return (Tag p (VarDecl (TPSymbol t) n (Just exp)), xs'')
    (Tag p _:_) -> fail $ show p ++ ": Expected semicolon here."
    [] -> fail $ show (pos exp) ++ ": EOF reached before semicolon to finish statement here."
parseStatement xx = do
  (Tag ep expr, xs) <- parseExpression xx
  case xs of
    (Tag _ Lex.Semicolon:xs') -> return (Tag ep (Expr expr), xs')
    (Tag p _:_) -> fail $ show p ++ ": Expected a semicolon here to finish the previous expression."
    [] -> fail $ show ep ++ ": EOF reached before semicolon to finish statement here."
--parseStatement [] = fail $ "EOF reached while parsing statement."

type Precedence = Int

{-
Precedence      Operator        Description
1          LTR  ++ --           Suffix/postfix increment and decrement
                ()              Function call
                []              Array subscripting
                .               Structure and union member access
                ->              Structure and union member access through pointer
                (type){list}    Compound literal(C99)
2          RTL  ++ --           Prefix increment and decrement
                + -             Unary plus and minus
                ! ~             Logical NOT and bitwise NOT
                (type)          Type cast
                *               Indirection (dereference)
                &               Address-of
                sizeof          Size-of[note 1]
                _Alignof        Alignment requirement(C11)
3          LTR  * / %           Multiplication, division, and remainder
4               + -             Addition and subtraction
5               << >>           Bitwise left shift and right shift
6               < <=            For relational operators < and ≤ respectively
                > >=            For relational operators > and ≥ respectively
7               == !=           For relational = and ≠ respectively
8               &               Bitwise AND
9               ^               Bitwise XOR (exclusive or)
10              |               Bitwise OR (inclusive or)
11              &&              Logical AND
12              ||              Logical OR
13[note 2] RTL  ?:              Ternary conditional[note 3]
14              =               Simple assignment
                += -=           Assignment by sum and difference
                *= /= %=        Assignment by product, quotient, and remainder
                <<= >>=         Assignment by bitwise left shift and right shift
                &= ^= |=        Assignment by bitwise AND, XOR, and OR
15         LTR  ,               Comma

ltr is a + b - c + d == ((a + b) - c) + d != a + (b - (c + d)) = a + b - c - d
rtl is a = b = c = d == a = (b = (c = d)) != ((a = b) = c) = d => a = d
((a + b) - c) + d
((a - b) + c) - d
(a , b) , c
-}

nullPrecedence :: Precedence
nullPrecedence = 16

precedence :: Operator -> Precedence
precedence Comma = 15
precedence Assign = 14
precedence Plus = 4
precedence Minus = 4
precedence Times = 3
precedence Divide = 3

isRightToLeft, isLeftToRight :: Precedence -> Bool
isRightToLeft t = t == 2 || t == 13 || t == 14
isLeftToRight = not . isRightToLeft

continue :: Precedence -> Precedence -> Bool
-- ltr is a + b - c + d == ((a + b) - c) + d != a + (b - (c + d))
-- rtl is a = b = c = d == a = (b = (c = d)) != ((a = b) = c) = d
continue l r = l > r || (l == r && isRightToLeft l)
