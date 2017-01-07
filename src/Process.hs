{-# LANGUAGE DeriveDataTypeable #-}
import WhileParser
import Data.Data
import Data.Maybe
import Control.Monad.State.Lazy
import qualified Data.Map.Strict as Map
data Variable = Nil
              | IntVar Integer
              | DoubleVar Double
              | BoolVar Bool
              | CharVar Char
              | StringVar String
              | PairVar Variable Variable
              | NilListVar
              deriving (Show, Typeable, Data) 
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
    Skip -> return ()
    (If cond stmt1 stmt2) ->
        do{
            val <- evalExpr cond;
            case val of
                (BoolVar x) -> if x then (evalStmt stmt1) else (evalStmt stmt2)
                otherwise -> error $ "incompatible type for condition variable: " ++ show (toConstr val)
        }
    (While cond stmt) ->
        do{
            val <- evalExpr cond;
            case val of
                (BoolVar x) -> if x then (evalStmt stmt) >> (evalStmt (While cond stmt)) else (evalStmt Skip)
                otherwise -> error $ "incompatible type for condition variable: " ++ show (toConstr val)
        }
    (ArrayDef arrayName expr) ->
        do{
            symbolTable <- get;
            length <- evalExpr expr;
            case length of
                (IntVar l) ->
                    do{
                        let
                            f 0 = symbolTable
                            f x = Map.insert (entryName arrayName (x - 1)) Nil (f (x - 1))
                        in
                            put(f l);
                        return ();
                    }
                otherwise -> error $ "incompatible type for array length: " ++ show (toConstr length)
        }
    (ArrayAssign arrayName expr1 expr2) ->
        do{
            symbolTable <- get;
            index <- evalExpr expr1;
            val <- evalExpr expr2;
            if toConstr(index) /= toConstr (IntVar undefined) then
                error $ "incompatible type for array index: " ++ show (toConstr index)
            else
                let
                    (IntVar x) = index
                    name = entryName arrayName x
                in 
                    if Map.notMember name symbolTable then
                        error $ "Variable not found: " ++ show arrayName ++ "[" ++ show index ++ "]"
                    else
                        put (Map.insert name val symbolTable);
        }

evalExpr :: Expr -> State SymbolTable Variable
evalExpr expr = case expr of
    (BoolLit x) -> return (BoolVar x)
    (IntLit x) -> return (IntVar x)
    (DoubleLit x) -> return (DoubleVar x)
    (CharLit x) -> return (CharVar x)
    (StringLit x) ->
        let
            f [] = NilListVar
            f (x:xs) = PairVar (CharVar x) (f xs)
        in
            return (f x)
    NilList -> return NilListVar
    (Pair expr1 expr2) ->
        do {
            val1 <- evalExpr expr1;
            val2 <- evalExpr expr2;
            return (PairVar val1 val2)
        }
    (PairFst expr) ->
        do {
            val <- evalExpr expr;
            case val of
                (PairVar x _) -> return x
                otherwise -> return (error $ "incompatible type for car: " ++ show (toConstr val))
        }
    (PairSnd expr) ->
        do {
            val <- evalExpr expr;
            case val of
                (PairVar _ x) -> return x
                otherwise -> return (error $ "incompatible type for cdr: " ++ show (toConstr val))
        }
    (Var varName) -> 
        do {
            symbolTable <- get;
            case (fromMaybe Nil (Map.lookup varName symbolTable)) of
                Nil -> return (error $ unwords ["Variable", show varName, "cannot be found!"])
                x@_ -> return x
        }
    (ArrayEntry arrayName expr) ->
        do {
            symbolTable <- get;
            index <- evalExpr expr;
            if toConstr(index) /= toConstr (IntVar undefined) then
                error $ "incompatible type for array index: " ++ show (toConstr index)
            else
                let
                    (IntVar x) = index
                    name = entryName arrayName x
                in 
                    case (fromMaybe Nil (Map.lookup name symbolTable)) of
                        Nil -> return (error $ "Variable not found: " ++ show arrayName ++ "[" ++ show index ++ "]")
                        x@_ -> return x
        }
    (ABinary op expr1 expr2) -> 
        do {
            val1 <- evalExpr expr1;
            val2 <- evalExpr expr2;
            return (evalAExpr op val1 val2);
        }
    (Not expr) ->
        do {
            val <- evalExpr expr;
            case val of
                (BoolVar x) -> return (BoolVar (not x));
                otherwise -> return (error $ unwords ["incompatible operands: Not", show (toConstr val)])
        }
    (BBinary op expr1 expr2) ->
        do {
            val1 <- evalExpr expr1;
            val2 <- evalExpr expr2;
            case (val1, val2) of
                (BoolVar x, BoolVar y) -> case op of
                    And -> return (BoolVar (x && y))
                    Or -> return (BoolVar (x || y))
                otherwise -> return (error $ unwords ["incompatible operands:", show op, show (toConstr val1), show (toConstr val2)])
        }
    (RBinary op expr1 expr2) ->
        do {
            val1 <- evalExpr expr1;
            val2 <- evalExpr expr2;
            return (evalRExpr op val1 val2);
        }

evalAExpr :: ABinOp -> Variable -> Variable -> Variable
evalAExpr op (IntVar val1) (IntVar val2) =
    case op of
        Add -> IntVar (val1 + val2)
        Subtract -> IntVar (val1 - val2)
        Multiply -> IntVar (val1 * val2)
        Divide -> IntVar (val1 `div` val2)
evalAExpr op (DoubleVar val1) (IntVar val2) = evalAExpr op (DoubleVar val1) (DoubleVar $ fromInteger val2)
evalAExpr op (IntVar val1) (DoubleVar val2) = evalAExpr op (DoubleVar $ fromInteger val1) (DoubleVar val2)
evalAExpr op (DoubleVar val1) (DoubleVar val2) =
    case op of
        Add -> DoubleVar (val1 + val2)
        Subtract -> DoubleVar (val1 - val2)
        Multiply -> DoubleVar (val1 * val2)
        Divide -> DoubleVar (val1 / val2)
evalAExpr op val1 val2 = error $ unwords ["incompatible operands:", show op, show (toConstr val1), show (toConstr val2)]

evalRExpr :: RBinOp -> Variable -> Variable -> Variable
evalRExpr op (IntVar val1) (IntVar val2) = evalRExpr op (DoubleVar $ fromInteger val1) (DoubleVar $ fromInteger val2)
evalRExpr op (DoubleVar val1) (IntVar val2) = evalRExpr op (DoubleVar val1) (DoubleVar $ fromInteger val2)
evalRExpr op (IntVar val1) (DoubleVar val2) = evalRExpr op (DoubleVar $ fromInteger val1) (DoubleVar val2)
evalRExpr op (DoubleVar val1) (DoubleVar val2) =
    case op of
        WhileParser.EQ -> BoolVar (val1 == val2)
        WhileParser.GE -> BoolVar (val1 >= val2)
        WhileParser.LE -> BoolVar (val1 <= val2)
        WhileParser.GT -> BoolVar (val1 > val2)
        WhileParser.LT -> BoolVar (val1 < val2)
evalRExpr op val1 val2 = error $ unwords ["incompatible operands:", show op, show (toConstr val1), show (toConstr val2)]

entryName :: String -> Integer -> String
entryName arrayName index = arrayName ++ "@" ++ show index


-- str = "(!set a 1)"
eval str = (execState (evalStmt $ parseString str)) (Map.empty)