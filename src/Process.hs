{-# LANGUAGE DeriveDataTypeable #-}
import WhileParser
import Data.Data
import Data.Maybe
import Control.Monad.State.Lazy
import qualified Data.Map.Strict as Map
data Variable = Nil | IntVar Integer | DoubleVar Double | BoolVar Bool deriving (Show, Typeable, Data)
type SymbolTable = Map.Map String Variable

evalStmt :: Stmt -> State SymbolTable ()
evalStmt stmt = case stmt of
    (StmtList []) -> return ()
    (StmtList (x:xs)) -> (evalStmt x) >> evalStmt (StmtList xs)
    (Assign varName expr) -> 
        do {
            symbolTable <- get;
            val <- evalExpr expr;
            put (Map.insert varName val symbolTable);
            return ();
        }

evalExpr :: Expr -> State SymbolTable Variable
evalExpr expr = case expr of
    (BoolConst x) -> return (BoolVar x)
    (IntConst x) -> return (IntVar x)
    (DoubleConst x) -> return (DoubleVar x)
    (Var varName) -> 
        do {
            symbolTable <- get;
            return (fromMaybe Nil (Map.lookup varName symbolTable));
        }
{-
    (ABinary aBinOp expr1 expr2) ->
        do {
            val1 <- evalExpr expr1
            val2 <- evalExpr expr2            
        }
    ABinary op expr1 expr2 -> 
        do
            (val1, val2) <- typeCheck (evalExpr expr1) (evalExpr val2)
            case op of
                Add -> return (val1 + val2)
                Subtract -> return (val1 - val2)
                Multiply -> return (val1 * val2)
                Divide -> return (val1 `div` val2)
-}
-- str = "(!set a 1)"
test str = (execState (evalStmt $ parseString str)) (Map.empty)