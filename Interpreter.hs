{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}

module Interpreter where

import Data.Maybe (fromJust, isNothing)
import Data.Map
import Control.Monad (when)
import Control.Monad.Reader
import Control.Monad.RWS.Lazy
import Control.Monad.Except
import qualified AbsLatte
import qualified Data.IntMap as Map
import AbsLatte (Type)

import TypeChecker

import Common

type Loc = Int

data Env = Env {
    _env :: Map AbsLatte.Ident Loc
} deriving (Show)

initEnv :: Env
initEnv = Env {
    _env = empty
}

data Store = Store {
    _store :: Map Loc Val
} deriving (Show)

initStore :: Store
initStore = Store {
    _store = empty
}

newLoc :: Eval Loc
newLoc = do
    store <- get
    let loc = length $ _store store
    return loc

data Val    = IntV  Integer
            | BoolV Bool
            | StrV String
            | FnV FnDef Env
            deriving (Show)

data FnDef = FnDef AbsLatte.Type [AbsLatte.Arg] AbsLatte.Block deriving (Show)

type Eval = ExceptT String (RWS Env [String] Store)

data RetType = Break | Continue | Return Val

type RetVal = Maybe RetType

runEval :: Env -> Store -> Eval a -> (Either String a, Store, [String])
runEval env store eval = runRWS (runExceptT eval) env store

runEvalProgram :: AbsLatte.Program -> IO ()
runEvalProgram program = do
    let (result, _, output) = runEval initEnv initStore (evalProgram program)
    case result of
        Left err -> putStrLn err
        Right _ -> mapM_ putStrLn output

evalProgram :: AbsLatte.Program -> Eval ()
evalProgram (AbsLatte.Program topDefs) = do
    env <- evalTopDefs topDefs
    store <- get
    evalMain env store

evalMain :: Env -> Store -> Eval ()
evalMain env store = local (const env) ( do
        ~(FnV (FnDef _ _  (AbsLatte.Block stmts)) mainEnv) <- getVarVal (AbsLatte.Ident "main")
        local (const mainEnv) (evalStmts stmts)
    )

evalTopDefs :: [AbsLatte.TopDef] -> Eval Env
evalTopDefs [] = ask
evalTopDefs (df:dfs) = do
    env <- evalTopDef df
    local (const env) (evalTopDefs dfs)

evalTopDef :: AbsLatte.TopDef -> Eval Env
evalTopDef (AbsLatte.FnDef retType ident args block) = do
    let fnDef = FnDef retType args block
    env <- ask
    newLoc <- newLoc
    let newEnv = Env $ insert ident newLoc (_env env)
    modify (Store . insert newLoc (FnV fnDef newEnv) . _store)
    return newEnv

evalStmts :: [AbsLatte.Stmt] -> Eval ()
evalStmts [] = return ()
evalStmts (stmt:stmts) = do
    env <- evalStmt stmt
    local (const env) (evalStmts stmts)

evalStmt :: AbsLatte.Stmt -> Eval Env
evalStmt stmt = if isDeclStmt stmt then evalDeclStmt stmt else evalNonDeclStmt stmt >> ask

evalNonDeclStmt :: AbsLatte.Stmt -> Eval ()
evalNonDeclStmt AbsLatte.Empty = return ()
evalNonDeclStmt (AbsLatte.BStmt (AbsLatte.Block stmts) ) = evalStmts stmts
evalNonDeclStmt (AbsLatte.Print expr) = do
    val <- evalExpr expr
    tell [show val]

evalDeclStmt :: AbsLatte.Stmt -> Eval Env
evalDeclStmt (AbsLatte.Decl _ items) = evalItems items

evalItems :: [AbsLatte.Item] -> Eval Env
evalItems [] = ask
evalItems (item:items) = do
    env <- evalItem item
    local (const env) (evalItems items)

evalItem :: AbsLatte.Item -> Eval Env
evalItem (AbsLatte.Init ident expr) = do
    env <- ask
    val <- evalExpr expr
    newLoc <- newLoc
    let newEnv = Env $ insert ident newLoc (_env env)
    modify (Store . insert newLoc val . _store)
    return newEnv
evalItem (AbsLatte.NoInit ident) = evalItem (AbsLatte.Init ident (AbsLatte.ELitInt 0))

evalExpr :: AbsLatte.Expr -> Eval Val
evalExpr (AbsLatte.EVar ident) = getVarVal ident
evalExpr (AbsLatte.ELitInt int) = return $ IntV int
evalExpr AbsLatte.ELitTrue = return $ BoolV True
evalExpr AbsLatte.ELitFalse = return $ BoolV False
evalExpr (AbsLatte.EString str) = return $ StrV str
evalExpr (AbsLatte.ELambda args block) = asks (FnV (FnDef AbsLatte.Void args block))
-- evalExpr (AbsLatte.EApp ident exprs) = do
--     ~(FnV (FnDef retType args (AbsLatte.Block stmts)) fnEnv) <- getVarVal ident
--     vals <- mapM evalExpr exprs
--     let newEnv = Env $ fromList $ zip (map (\(AbsLatte.Arg _ ident) -> ident) args) (map IntV vals)
--     local (const newEnv) (evalStmts stmts)
--     return $ case retType of
--         AbsLatte.Int -> IntV 0
--         AbsLatte.Bool -> BoolV False
--         AbsLatte.Str -> StrV ""
--         AbsLatte.Void -> IntV 0
--         _ -> error "Unknown return type"

evalExpr (AbsLatte.Not expr) = do
    val <- evalExpr expr
    case val of
        BoolV bool -> return $ BoolV (not bool)
        _ -> throwError "Negating non-boolean value"

evalExpr (AbsLatte.EAnd expr1 expr2) = evalBoolOp expr1 expr2 (&&)
evalExpr (AbsLatte.EOr expr1 expr2) = evalBoolOp expr1 expr2 (||)

evalExpr (AbsLatte.Neg expr) = do
    val <- evalExpr expr
    case val of
        IntV int -> return $ IntV (-int)
        _ -> throwError "Negating non-integer value"

evalExpr (AbsLatte.EMul expr1 AbsLatte.Times expr2) = evalIntArithmeticOp expr1 expr2 (*)
evalExpr (AbsLatte.EMul expr1 AbsLatte.Div expr2) = evalIntArithmeticOp expr1 expr2 div
evalExpr (AbsLatte.EMul expr1 AbsLatte.Mod expr2) = evalIntArithmeticOp expr1 expr2 mod
evalExpr (AbsLatte.EAdd expr1 AbsLatte.Plus expr2) = evalIntArithmeticOp expr1 expr2 (+)
evalExpr (AbsLatte.EAdd expr1 AbsLatte.Minus expr2) = evalIntArithmeticOp expr1 expr2 (-)

evalExpr (AbsLatte.ERel expr1 AbsLatte.LTH expr2) = evalIntRelOp expr1 expr2 (<)
evalExpr (AbsLatte.ERel expr1 AbsLatte.LE expr2) = evalIntRelOp expr1 expr2 (<=)
evalExpr (AbsLatte.ERel expr1 AbsLatte.GTH expr2) = evalIntRelOp expr1 expr2 (>)
evalExpr (AbsLatte.ERel expr1 AbsLatte.GE expr2) = evalIntRelOp expr1 expr2 (>=)
evalExpr (AbsLatte.ERel expr1 AbsLatte.EQU expr2) = evalIntRelOp expr1 expr2 (==)
evalExpr (AbsLatte.ERel expr1 AbsLatte.NE expr2) = evalIntRelOp expr1 expr2 (/=)

evalBoolOp :: AbsLatte.Expr -> AbsLatte.Expr -> (Bool -> Bool -> Bool) -> Eval Val
evalBoolOp expr1 expr2 op = do
    val1 <- evalExpr expr1
    val2 <- evalExpr expr2
    case (val1, val2) of
        (BoolV bool1, BoolV bool2) -> return $ BoolV (op bool1 bool2)
        _ -> throwError "Non-boolean value in boolean operation"

evalIntArithmeticOp :: AbsLatte.Expr -> AbsLatte.Expr -> (Integer -> Integer -> Integer) -> Eval Val
evalIntArithmeticOp exp1 exp2 f = do
    val1 <- evalExpr exp1
    val2 <- evalExpr exp2
    case (val1, val2) of
        (IntV int1, IntV int2) -> return $ IntV (f int1 int2)
        _ -> throwError "Invalid operands for binary operation"

evalIntRelOp :: AbsLatte.Expr -> AbsLatte.Expr -> (Integer -> Integer -> Bool) -> Eval Val
evalIntRelOp exp1 exp2 f = do
    val1 <- evalExpr exp1
    val2 <- evalExpr exp2
    case (val1, val2) of
        (IntV int1, IntV int2) -> return $ BoolV (f int1 int2)
        _ -> throwError "Invalid operands for binary operation"

getVarVal :: AbsLatte.Ident -> Eval Val
getVarVal ident = do
    loc <- getVarLoc ident
    getLocVal loc

getVarLoc :: AbsLatte.Ident -> Eval Loc
getVarLoc ident = do
    env <- ask
    let maybeLoc = Data.Map.lookup ident $ _env env
    when (isNothing maybeLoc) $ throwError $ "Variable " ++ show ident ++ " not defined"
    return $ fromJust maybeLoc

getLocVal :: Loc -> Eval Val
getLocVal loc = do
    store <- get
    let maybeVal = Data.Map.lookup loc $ _store store
    when (isNothing maybeVal) $ throwError $ "Accessing undefined location " ++ show loc
    return $ fromJust maybeVal