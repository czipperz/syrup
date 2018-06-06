module Eval where

import Parse
import Pos
import Value
import Foreign.Storable
import Foreign.Ptr
import Control.Applicative
import Type
import TypeCheck
import Result
import Operator
import qualified Lex

data Environment = Environment [(String, Reference)] [(String, Reference)] deriving Show

evalStatementsString :: Environment -> String -> IO Environment
evalStatementsString env str =
  case (do stmts <- parseStatements =<< Lex.tokens "*stdin*" str
           (_, stmts') <- typeCheckStatements emptyTypeCache stmts
           return stmts') of
    Ok stmts -> evalStatements env stmts
    Err err -> fail err

evalExpressionString :: String -> IO MaybeReference
evalExpressionString s =
  case (do e <- fmap fst . parseExpression =<< Lex.tokens "*stdin*" s
           (_, _, te) <- typeCheckExpression emptyTypeCache e
           return $ Tag (pos e) te) of
    Ok e -> fmap snd $ evalExpression emptyEnv e
    Err err -> fail err

evalExpressionStringEnv :: Environment -> String -> IO (Environment, MaybeReference)
evalExpressionStringEnv env@(Environment locals globals) s =
  case (do e <- fmap fst . parseExpression =<< Lex.tokens "*stdin*" s
           (_, _, te) <- typeCheckExpression typeCache e
           return $ Tag (pos e) te) of
    Ok e -> evalExpression env e
    Err err -> fail err
  where typeCache = TypeCache . map (fmap typeOfReference) $ locals ++ globals

emptyEnv :: Environment
emptyEnv = Environment [] []

lookupEnv :: String -> Environment -> IO WrappedValue
lookupEnv var env = applyEnv (Tag undefined var) env >>= evalReference

applyEnv :: Tag String -> Environment -> IO Reference
applyEnv (Tag p var) (Environment locals globals) =
  case lookup var locals <|> lookup var globals of
    Just x -> return x
    Nothing -> fail $ show p ++ ": Unbound variable " ++ var

extendEnvLocal :: String -> WrappedValue -> Environment -> IO Environment
extendEnvLocal name value (Environment locals globals) = do
  ref <- newReference value
  return $ Environment ((name, ref):locals) globals

setEnvLocal :: String -> WrappedValue -> Environment -> IO ()
setEnvLocal name value (Environment locals _) = do
  case lookup name locals of
    Just ref -> setReference ref value
    Nothing -> fail $ "setEnvLocal: Unbound variable " ++ name

extendEnvGlobal :: String -> WrappedValue -> Environment -> IO Environment
extendEnvGlobal name value (Environment locals globals) = do
  ref <- newReference value
  return $ Environment locals ((name, ref):globals)

cleanupEnv :: Environment -> IO ()
cleanupEnv (Environment locals globals) = mapM_ freeReference . map snd $ locals ++ globals

{- |
>>> evalStatementsString emptyEnv "int x; x = 3;" >>= lookupEnv "x"
3
>>> evalStatementsString emptyEnv "int x = 3;" >>= lookupEnv "x"
3
>>> evalStatementsString emptyEnv "int x = 13; x = 3 + 1;" >>= \env -> fmap snd (evalExpressionStringEnv env "x + 3") >>= evalMaybeReference
7
>>> evalStatementsString emptyEnv "int x = 3 + 2; x = x + 1;" >>= lookupEnv "x"
6
>>> evalStatementsString emptyEnv "int x = 3 + 2 / 2 * 3;" >>= lookupEnv "x"
6
>>> evalStatementsString emptyEnv "int* x = 0;" >> return ()
>>> evalStatementsString emptyEnv "char* x = \"hello\";" >>= lookupEnv "x"
"hello"
-}
evalStatements :: Environment -> [Tag (Statement Type)] -> IO Environment
evalStatements env [] = return env
evalStatements env [x] = fmap fst $ evalStatement env x
evalStatements env (x:xs) = do
  (env', _) <- evalStatement env x
  evalStatements env' xs

evalStatement :: Environment -> Tag (Statement Type) -> IO (Environment, MaybeReference)
evalStatement env (Tag _ (Braced b)) = do
  env' <- evalStatements env b
  return (env', Val WVVoid)
evalStatement env (Tag p (Expr e)) = evalExpression env (Tag p e)
evalStatement env (Tag p (VarDecl t n exp)) = do
  env' <- extendEnvLocal n (undefinedWValue t) env
  case exp of
    Just exp' -> do
      (env'', ref) <- evalExpression env' exp'
      v <- evalMaybeReference ref
      setEnvLocal n v env''
      return (env'', Val v)
    Nothing -> return (env', Val WVVoid)
evalStatement env (Tag _ EmptyStatement) = return (env, Val WVVoid)

{- |
>>> evalExpressionString "1 + 3"
4
>>> evalExpressionString "1 + 3 - 2 + 5"
7
>>> evalExpressionString "1 + 3 - (2 + 5)"
-3
>>> evalExpressionString "\"asdf\\n\""
"asdf\n"
>>> evalExpressionString "\"asdf\\n\" + 2"
"df\n"
-}
evalExpression :: Environment -> Tag (Expression Type) -> IO (Environment, MaybeReference)
evalExpression env (Tag p (Var x)) = do
  v <- applyEnv (Tag p x) env
  return (env, Ref v)
evalExpression env (Tag _ (Value v)) = return (env, Val v)
evalExpression _ (Tag _ (FunctionCall _ _)) = undefined
-- evalExpression env (Tag p (Index a b)) = do
--   (a', env') <- evalExpression env a
--   (b', env') <- evalExpression env b
--   case a' of
--     VArray v -> case b' of
--                   VInt i -> return (v !! i)
evalExpression env (Tag _ (Paren p)) = evalExpression env p
evalExpression env (Tag _ (InfixOperator op a b)) = do
  (env', ra) <- evalExpression env a
  (env'', rb) <- evalExpression env' b
  case op of
    Comma -> return (env'', rb)
    Assign -> do
      vb <- evalMaybeReference rb
      let Ref (Reference ptr _) = ra
      case vb of
        WVInt i -> do poke (castPtr ptr) i
                      return (env'', Val vb)
    Plus -> do
      va <- evalMaybeReference ra
      vb <- evalMaybeReference rb
      case (va, vb) of
        (WVInt ia, WVInt ib) -> return (env'', Val . WVInt $ ia + ib)
        (WVString ia, WVInt ib) -> return (env'', Val . WVString $ drop ib ia)
    Minus -> do
      va <- evalMaybeReference ra
      vb <- evalMaybeReference rb
      case (va, vb) of
        (WVInt ia, WVInt ib) -> return (env'', Val . WVInt $ ia - ib)
        (WVString ia, WVInt ib) -> undefined
    Times -> do
      va <- evalMaybeReference ra
      vb <- evalMaybeReference rb
      case (va, vb) of
        (WVInt ia, WVInt ib) -> return (env'', Val . WVInt $ ia * ib)
    Divide -> do
      va <- evalMaybeReference ra
      vb <- evalMaybeReference rb
      case (va, vb) of
        (WVInt ia, WVInt ib) -> return (env'', Val . WVInt $ ia `div` ib)
