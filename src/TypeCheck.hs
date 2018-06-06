module TypeCheck where

import Pos
import Result
import Value
import Parse
import Type
import Control.Monad (unless)
import Operator

data TypeCache = TypeCache [(String, Type)]
emptyTypeCache :: TypeCache
emptyTypeCache = TypeCache []

lookupType :: Tag String -> Result Type
lookupType (Tag _ "int") = return TInt
lookupType (Tag _ "void") = return TVoid
lookupType (Tag p s) = fail $ show p ++ ": Undeclared type " ++ s

extendTypeCache :: Type -> String -> TypeCache -> TypeCache
extendTypeCache t s (TypeCache vars) = TypeCache ((s, t):vars)

lookupVar :: TypeCache -> Tag String -> Result Type
lookupVar (TypeCache vars) (Tag p s) =
  case lookup s vars of
    Just t -> return t
    Nothing -> fail $ show p ++ ": Unbound variable " ++ s

canConvert :: Type -> Type -> Bool
canConvert a b | a == b = True
canConvert _ _ = False

typeCheckStatements :: TypeCache -> [Tag (Statement TypePlaceholder)] -> Result (TypeCache, [Tag (Statement Type)])
typeCheckStatements typeCache [] = return (typeCache, [])
typeCheckStatements typeCache (x:xs) = do
  (typeCache', x') <- typeCheckStatement typeCache x
  fmap (fmap (x':)) $ typeCheckStatements typeCache' xs

typeCheckStatement :: TypeCache -> Tag (Statement TypePlaceholder) -> Result (TypeCache, Tag (Statement Type))
typeCheckStatement typeCache (Tag p (Braced list)) =
  fmap (fmap (Tag p . Braced)) $ typeCheckStatements typeCache list
typeCheckStatement typeCache (Tag p (Expr e)) = do
  (_, _, e') <- typeCheckExpression typeCache (Tag p e)
  return (typeCache, Tag p (Expr e'))
typeCheckStatement typeCache (Tag p (VarDecl (TPSymbol t) s v)) = do
  t' <- lookupType (Tag p t)
  case t' of
    TVoid -> fail $ show p ++ ": A variable Cannot have a"
    _ -> return ()
  let typeCache' = extendTypeCache t' s typeCache
  case v of
    Just v' -> do
      (vt, _, v'') <- typeCheckExpression typeCache' v'
      v''' <- if vt == t' then
                return (Just (Tag (pos v') v''))
              else if canConvert vt t' then
                return (Just (Tag (pos v') (TypeCast t' (Tag (pos v') v''))))
              else
                fail $ show p ++ ": Value of variable can not be converted to it's type"
      return (typeCache', Tag p (VarDecl t' s v'''))
    Nothing -> return (typeCache', Tag p (VarDecl t' s Nothing))
typeCheckStatement typeCache (Tag p EmptyStatement) = return (typeCache, Tag p EmptyStatement)

{- |
>>> typeCheckExpression emptyTypeCache =<< fmap fst . parseExpression =<< Lex.tokens "*stdin*" "x = 5;"
*stdin*:1:1: Unbound variable x
>>> typeCheckExpression emptyTypeCache =<< fmap fst . parseExpression =<< Lex.tokens "*stdin*" "3 = 5;"
*stdin*:1:1: Cannot assign to a value
-}
typeCheckExpression :: TypeCache -> Tag (Expression TypePlaceholder) -> Result (Type, Bool, Expression Type)
typeCheckExpression typeCache (Tag p (Var s)) = fmap (\t -> (t, True, Var s)) $ lookupVar typeCache (Tag p s)
typeCheckExpression _ (Tag _ (Value v)) =
  return (case v of (WVInt _) -> TInt
                    (WVChar _) -> TChar
                    (WVString _) -> TPointer TChar
                    WVVoid -> TVoid,
          False, Value v)
typeCheckExpression typeCache (Tag _ (FunctionCall name args)) = undefined
typeCheckExpression typeCache (Tag _ (Index a b)) = undefined
typeCheckExpression typeCache (Tag _ (Paren a)) = typeCheckExpression typeCache a
typeCheckExpression typeCache (Tag _ (InfixOperator op a b)) = do
  (typeA, isRefA, exprA) <- typeCheckExpression typeCache a
  (typeB, isRefB, exprB) <- typeCheckExpression typeCache b
  let e = InfixOperator op (Tag (pos a) exprA) (Tag (pos b) exprB)
  let e'= InfixOperator op (Tag (pos b) exprB) (Tag (pos a) exprA)
  case (op, typeA, typeB) of
    (Comma, _, _) -> return (typeB, isRefB, e)
    (_, TVoid, _) -> fail $ show (pos a) ++ ": Binary operations cannot have void values"
    (_, _, TVoid) -> fail $ show (pos b) ++ ": Binary operations cannot have void values"
    (Plus, TInt, TInt) -> return (TInt, False, e)
    (Plus, p@(TPointer _), TInt) -> return (p, False, e)
    (Plus, TInt, p@(TPointer _)) -> return (p, False, e')
    (Minus, TInt, TInt) -> return (TInt, False, e)
    (Minus, p@(TPointer _), TInt) -> return (p, False, e)
    (Minus, TInt, p@(TPointer _)) -> return (p, False, e')
    (Times, TInt, TInt) -> return (TInt, False, e)
    (Divide, TInt, TInt) -> return (TInt, False, e)
    (Assign, TInt, TInt) -> if isRefA then return (TInt, False, e) else fail $ show (pos a) ++ ": Cannot assign to a value"
    (Assign, var@(TPointer _), val@(TPointer _)) -> if isRefA then return (val, False, e) else fail $ show (pos a) ++ ": Cannot assign to a value"
