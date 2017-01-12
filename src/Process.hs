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
              | ArrayVar (Int, Map.Map Int Variable)
              deriving (Show, Typeable, Data) 
type SymbolTable = Map.Map String Variable
type SymbolStack = [(String, Variable)]
type FuncList = Map.Map String FuncDecl
type SymState = (SymbolTable, SymbolStack, FuncList)

-- Functions to wrap the original symbol table, the new Array implementation and the list of functions for call.
-- Note: By convention the latest Let clause are the first in the stack
-- Note that in assignments, we don't need to consider the stack(for let clauses)
symUpdate :: SymState -> String -> Variable -> SymState
symUpdate (t, s, fl) name expr = ((Map.insert name expr t), s, fl)

symArrUpdate :: SymState -> String -> Int -> Variable -> SymState
symArrUpdate (t, s, fl) name idx expr = (nt, s, fl) where
       nt = case (Map.lookup name t) of
            Nothing -> error $ "Variable not found: " ++ show name
            Just (ArrayVar (len, content)) ->
                if ((idx >= 0) && (idx < len))
                then let updated = (ArrayVar (len, (Map.insert idx expr content))) in (Map.insert name updated t)
                else error $ "Illegal subscription " ++ show idx ++ " in " ++ show name ++ "[" ++ show len ++ "]"
            otherwise -> error $ "Variable is not an array: " ++ show name

stackLookup :: SymbolStack -> String -> Maybe Variable
stackLookup [] _ = Nothing
stackLookup (x:xs) cur = let name = fst x; var = snd x in if name == cur then (Just var) else stackLookup xs cur

symLookup :: SymState -> String -> Maybe Variable
symLookup (t, s, _) name = case stackLookup s name of
                        Nothing -> Map.lookup name t
                        Just v -> Just v

symArrLookup :: SymState -> String -> Int -> Maybe Variable
symArrLookup state@(t, s, _) name idx = case symLookup state name of
                               Nothing -> error $ "Variable not found: " ++ show name
                               Just (ArrayVar (len, content)) -> if ((idx >= 0) && (idx < len)) then (Map.lookup idx content)
                                                                else error $ "Illegal subscription " ++ show idx ++ " in " ++ show name ++ "[" ++ show len ++ "]"
                               otherwise -> error $ "Variable is not an array: " ++ show name

-- Functionality for function calls.
-- Since "return" itself is a reserved key, we use it to store the actual return value of the function

-- Find the function.
findFunc :: SymState -> String -> Maybe FuncDecl
findFunc (_, _, fl) fname = Map.lookup fname fl

-- Before a function call, return the symbol table containing its parameters. The old state is a parameter to fetch function list.
buildCall :: SymState -> [String] -> [Variable] -> SymState
buildCall (_, _, fl) names vars = (nt, [], fl) where nt = if (length names) == (length vars) then Map.fromList (zip names vars)
                                                          else error $ "Parameter length mismatch: " ++ show (length names) ++ " parameters and provided " ++ show (length vars)

-- Is a return value assigned?
isRet :: SymState -> Bool
isRet (t, _, _) = Map.member "return" t

-- Functionality for let.. calls.

bindVar :: SymState -> String -> Variable -> SymState
bindVar (t, s, fl) name val = (t, (name, val):s, fl)

unbindVar :: SymState -> SymState
unbindVar (t, [], fl) = error $ "Internal Error - Empty Stack in Let clause"
unbindVar (t, (_:xs), fl) = (t, xs, fl)



evalStmt :: Stmt -> State SymState ()
evalStmt stmt = case stmt of
    (StmtList []) -> return ()
    (StmtList (x:xs)) -> do {
        evalStmt x;
        sym <- get;
        if isRet sym then return ()  else evalStmt (StmtList xs)
    }
    (Assign varName expr) -> 
        do {
            symbolTable <- get;
            val <- evalExpr expr;
            put (symUpdate symbolTable varName val);
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
                (BoolVar x) -> if x then do {
                    evalStmt stmt;
                    sym <- get;
                    if isRet sym then return () else evalStmt (While cond stmt)
                    }
                    else (evalStmt Skip)
                otherwise -> error $ "incompatible type for condition variable: " ++ show (toConstr val)
        }
    (ArrayDef arrayName expr) ->
        do{
            symbolTable <- get;
            length <- evalExpr expr;
            case length of
                (IntVar l) -> put (symUpdate symbolTable arrayName (ArrayVar (fromIntegral l, Map.fromList [])));
                otherwise -> error $ "incompatible type for array length: " ++ show (toConstr length)
        }
    (ArrayAssign arrayName expr1 expr2) ->
        do{
            symbolTable <- get;
            index <- evalExpr expr1;
            val <- evalExpr expr2;
            case index of
                (IntVar l) -> put (symArrUpdate symbolTable arrayName (fromIntegral l) val);
                otherwise -> error $ "incompatible type for array index: " ++ show (toConstr index)
        }
    (Return expr) ->
        do {
            symbolTable <- get;
            val <- evalExpr expr;
            put (symUpdate symbolTable "return" val)
        }

evalExpr :: Expr -> State SymState Variable
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
            case symLookup symbolTable varName of
                Just x -> return x
                Nothing -> return (error $ "Can't find variable " ++ show(varName))
        }
    (ArrayEntry arrayName expr) ->
        do {
            symbolTable <- get;
            index <- evalExpr expr;
            case index of
                (IntVar xlong) -> let x = fromIntegral xlong in
                                    case symArrLookup symbolTable arrayName x of
                                    Just x -> return x
                                    Nothing -> return (error $ "Array entry is not initialized: " ++ show(arrayName) ++ "@" ++ show(x))
                otherwise -> return (error $ "Incompatible type for subscription: " ++ show (toConstr index))
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
    (Call funcname params) ->
        do {
            sym <- get;
            case findFunc sym funcname of
            Just f -> do {
                vals <- evalExprList params;
                ret <- evalFunc f vals;
                return ret
            }
            Nothing -> return (error $ "Function name not found: " ++ show(funcname))
        }
    (Let varName varExpr expr) ->
        do {
            sym <- get;
            value <- evalExpr varExpr;
            put (bindVar sym varName value);
            ret <- evalExpr expr;
            sym <- get;
            put (unbindVar sym);
            return ret
        }

evalExprList :: [Expr] -> State SymState [Variable]
evalExprList [] = return []
evalExprList (x:xs) = do {
                        r <- evalExpr x;
                        rs <- evalExprList xs;
                        return (r:rs)
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

evalFunc :: FuncDecl -> [Variable] -> State SymState Variable
evalFunc f params = case f of
                Function fn args body -> do{
                    old_sym <- get;
                    put (buildCall old_sym args params);
                    evalStmt body;
                    new_sym <- get;
                    put old_sym;
                    case symLookup new_sym "return" of
                        (Just x) -> return x
                        Nothing -> return (error $ "No return statement executing " ++ show(fn) ++ show(args))
                }

getFuncName :: FuncDecl -> String
getFuncName (Function fn _ _) = fn

evalProg :: ProgDecl -> Variable
evalProg p = case p of
                Program fs -> evalState (evalFunc fmain []) (Map.empty, [], funcmap)
                        where funcmap = Map.fromList [(getFuncName f, f) | f <- fs]
                              fmain = if Map.notMember "main" funcmap then (error "No main function found")
                                                                      else fromJust (Map.lookup "main" funcmap)

runProg :: String -> Variable
runProg str = evalProg $ parseProgramStr str

runStmt :: String -> SymState
runStmt str = execState (evalStmt $ parseString str) (Map.empty, [], Map.empty)

-- str = "(!set a 1)"
-- eval str = (execState (evalStmt $ parseString str)) (Map.empty)

test_ret = "(define (main) (begin (return 10)))" -- Outputs 10
test_call = "(define (test x y z) (begin (return (let q (+ x y) (+ z q))) )) (define (main) (return (test 5 10 15) ) )" -- Outputs 30
test_earlyhalt = "(define (main) (begin (set! x 10) (while (> x 0) (begin (set! x (- x 1)) (return x) ) ) ) )" -- Outputs 9 instead of 0
test_assign = "(define (main) (begin (make-vector a 10) (vector-set! a 5 10) (return (vector-ref a 5))) )" -- Outputs 10
test_miss = "(define (main) (begin (make-vector a 10) (vector-set! a 5 10) (return (vector-ref a 6))) )" -- Reports no initialization
test_bound = "(define (main) (begin (make-vector a 10) (vector-set! a 10 10)) )" -- Reports out of bound
test_missret = "(define (main) (set! x 10) )" -- Reports no return value
