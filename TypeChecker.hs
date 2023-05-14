{-# LANGUAGE TupleSections #-}
module TypeChecker where

import Data.Maybe (fromJust, isNothing)
import Data.Map
import Control.Monad (when)
import Control.Monad.Reader
import Control.Monad.Except
import qualified AbsLatte
import qualified Data.IntMap as Map
import AbsLatte (Type)

import Common
import Control.Monad.RWS (MonadState(get))

data TypeEnv = TypeEnv {
    types :: Map AbsLatte.Ident Type,
    currentReturnType :: Type
} deriving (Show)

insertType :: AbsLatte.Ident -> Type -> TypeEnv -> TypeEnv
insertType ident t env = env {
    types = insert ident t (types env)
}

insertTypes :: [(AbsLatte.Ident, Type)] -> TypeEnv -> TypeEnv
insertTypes ts env = env {
    types = fromList ts `union` types env
}

insertCurrentReturnType :: Type -> TypeEnv -> TypeEnv
insertCurrentReturnType t env = env {
    currentReturnType = t
}

getFunctionTypeEnv :: AbsLatte.Type -> [AbsLatte.Arg] -> TypeEnv -> TypeEnv
getFunctionTypeEnv returnType args env = insertTypes (Prelude.map (\(AbsLatte.Arg t ident) -> (ident, t)) args) (insertCurrentReturnType returnType env)

initTypeEnv :: TypeEnv
initTypeEnv = TypeEnv {
    types = empty,
    currentReturnType = AbsLatte.Void
}

type TypeCheck = ExceptT String (Reader TypeEnv)

runTypeCheck :: TypeEnv -> TypeCheck a -> Either String a
runTypeCheck env typeCheck = runReader (runExceptT typeCheck) env

runTypeCheckProgram :: AbsLatte.Program -> Either String ()
runTypeCheckProgram program = runTypeCheck initTypeEnv (typeCheckProgram program)

typeCheckProgram :: AbsLatte.Program -> TypeCheck ()
typeCheckProgram (AbsLatte.Program topDefs) = typeCheckTopDefs topDefs

typeCheckTopDefs :: [AbsLatte.TopDef] -> TypeCheck ()
typeCheckTopDefs [] = return ()
typeCheckTopDefs (topDef:topDefs) = do
    env <- typeCheckTopDef topDef
    local (const env) (typeCheckTopDefs topDefs)

typeCheckTopDef :: AbsLatte.TopDef -> TypeCheck TypeEnv
typeCheckTopDef (AbsLatte.FnDef t ident args (AbsLatte.Block stmts)) = do
    env <- ask
    let fnEnv = getFunctionTypeEnv t args env
    local (const fnEnv) $ typeCheckStmts stmts
    let argTypes = Prelude.map (\(AbsLatte.Arg t _) -> t) args
    let fnType = AbsLatte.Fn t argTypes
    let newEnv = insertType ident fnType initTypeEnv
    return newEnv

typeCheckStmts :: [AbsLatte.Stmt] -> TypeCheck ()
typeCheckStmts [] = return ()
typeCheckStmts (stmt:stmts) = do
    env <- typeCheckStmt stmt
    local (const env) (typeCheckStmts stmts)

typeCheckStmt :: AbsLatte.Stmt -> TypeCheck TypeEnv
typeCheckStmt stmt = if isDeclStmt stmt then typeCheckDeclStmt stmt else typeCheckNonDeclStmt stmt >> ask

typeCheckNonDeclStmt :: AbsLatte.Stmt -> TypeCheck ()
typeCheckNonDeclStmt AbsLatte.Empty = return ()
typeCheckNonDeclStmt (AbsLatte.BStmt (AbsLatte.Block stmts)) = typeCheckStmts stmts
typeCheckNonDeclStmt (AbsLatte.Print _) = return ()
typeCheckNonDeclStmt (AbsLatte.While expr stmt) = do
    exprType <- typeCheckExpr expr
    when (exprType /= AbsLatte.Bool) $ throwError $ "Type mismatch in while condition: Bool expected, " ++ show exprType ++ " found"
    typeCheckStmt stmt
    return ();
typeCheckNonDeclStmt (AbsLatte.Cond expr stmt) = do
    exprType <- typeCheckExpr expr
    when (exprType /= AbsLatte.Bool) $ throwError $ "Type mismatch in if condition: Bool expected, " ++ show exprType ++ " found"
    typeCheckStmt stmt
    return ();
typeCheckNonDeclStmt (AbsLatte.CondElse expr stmt1 stmt2) = do
    exprType <- typeCheckExpr expr
    when (exprType /= AbsLatte.Bool) $ throwError $ "Type mismatch in if condition: Bool expected, " ++ show exprType ++ " found"
    typeCheckStmt stmt1
    typeCheckStmt stmt2
    return ();
typeCheckNonDeclStmt (AbsLatte.SExp expr) = do
    typeCheckExpr expr
    return ();
typeCheckNonDeclStmt (AbsLatte.Ass ident expr) = do
    varType <- typeCheckExpr (AbsLatte.EVar ident)
    exprType <- typeCheckExpr expr
    when (varType /= exprType) $ throwError $ "Type mismatch in assignment: " ++ show varType ++ " expected " ++ show exprType ++ " found"

typeCheckDeclStmt :: AbsLatte.Stmt -> TypeCheck TypeEnv
typeCheckDeclStmt (AbsLatte.Decl t items) = do
    mapM_ (typeCheckInit t) $ Prelude.filter isInit items
    let idents = Prelude.map extractIdent items
    let itemEnv = fromList $ Prelude.map (, t) idents
    env <- ask
    let newEnv = insertTypes (toList itemEnv) env
    return newEnv

typeCheckInit :: Type -> AbsLatte.Item -> TypeCheck ()
typeCheckInit t (AbsLatte.Init ident expr) = do
    exprType <- typeCheckExpr expr
    when (exprType /= t) $ throwError $ "Type mismatch in initialization of " ++ show ident ++ ": " ++ show t ++ " expected, " ++ show exprType ++ " found"

isInit :: AbsLatte.Item -> Bool
isInit (AbsLatte.Init _ _) = True
isInit _ = False

extractIdent :: AbsLatte.Item -> AbsLatte.Ident
extractIdent (AbsLatte.NoInit ident) = ident
extractIdent (AbsLatte.Init ident _) = ident

typeCheckExpr :: AbsLatte.Expr -> TypeCheck Type
typeCheckExpr (AbsLatte.EVar ident) = do
    env <- ask
    let maybeType = Data.Map.lookup ident $ types env
    when (isNothing maybeType) $ throwError $ "Variable " ++ show ident ++ " not defined"
    return $ fromJust maybeType
typeCheckExpr AbsLatte.ELitInt{} = return AbsLatte.Int
typeCheckExpr AbsLatte.ELitTrue{} = return AbsLatte.Bool
typeCheckExpr AbsLatte.ELitFalse{} = return AbsLatte.Bool
typeCheckExpr AbsLatte.EString{} = return AbsLatte.Str
-- typeCheckExpr (AbsLatte.ELambda t args (AbsLatte.Block stmts)) = do
--     env <- ask
--     let fnEnv = getFunctionTypeEnv t args env
--     local (const fnEnv) $ typeCheckStmts stmts
--     let argTypes = Prelude.map (\(AbsLatte.Arg t _) -> t) args
--     let fnType = AbsLatte.Fn t argTypes
--     return fnType

typeCheckExpr (AbsLatte.EApp ident exprs) = do
    env <- ask
    let maybeType = Data.Map.lookup ident $ types env
    when (isNothing maybeType) $ throwError $ "Function " ++ show ident ++ " not defined"
    let varType = fromJust maybeType
    unless (isFnType varType) $ throwError $ "Variable " ++ show ident ++ " is not a function"
    let (AbsLatte.Fn returnType argTypes) = varType
    when (length exprs /= length argTypes) $ throwError $ "Wrong number of arguments in function call: " ++ show ident
    argTypes' <- mapM typeCheckExpr exprs
    mapM_ (\(t1, t2) -> when (t1 /= t2) $ throwError $ "Type mismatch in function call: " ++ show ident) $ zip argTypes argTypes'
    return returnType

typeCheckExpr (AbsLatte.Not expr) = do
    exprType <- typeCheckExpr expr
    when (exprType /= AbsLatte.Bool) $ throwError $ "Type mismatch in negation: " ++ show expr
    return AbsLatte.Bool

typeCheckExpr (AbsLatte.EAnd expr1 expr2) = typeCheckOp "&&" AbsLatte.Bool AbsLatte.Bool expr1 expr2
typeCheckExpr (AbsLatte.EOr expr1 expr2) = typeCheckOp "||" AbsLatte.Bool AbsLatte.Bool expr1 expr2

typeCheckExpr (AbsLatte.Neg expr) = do
    exprType <- typeCheckExpr expr
    when (exprType /= AbsLatte.Int) $ throwError $ "Type mismatch in negation: " ++ show expr
    return AbsLatte.Int

typeCheckExpr (AbsLatte.EMul expr1 op expr2) = typeCheckOp op AbsLatte.Int AbsLatte.Int expr1 expr2
typeCheckExpr (AbsLatte.EAdd expr1 op expr2) = typeCheckOp op AbsLatte.Int AbsLatte.Int expr1 expr2
typeCheckExpr (AbsLatte.ERel expr1 op expr2) = typeCheckOp op AbsLatte.Int AbsLatte.Bool expr1 expr2

typeCheckOp :: Show a => a -> AbsLatte.Type -> AbsLatte.Type -> AbsLatte.Expr -> AbsLatte.Expr -> TypeCheck Type
typeCheckOp op argType resType expr1 expr2 = do
    exprType1 <- typeCheckExpr expr1
    exprType2 <- typeCheckExpr expr2
    when (exprType1 /= argType || exprType2 /= argType) $ throwError $ "Type mismatch in : " ++ show expr1 ++ " " ++ show op ++ " " ++ show expr2
    return resType

isFnType :: Type -> Bool
isFnType (AbsLatte.Fn _ _) = True
isFnType _ = False

getFnType :: Type -> Maybe Type
getFnType (AbsLatte.Fn t _) = Just t
getFnType _ = Nothing