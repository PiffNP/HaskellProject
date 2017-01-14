{-# LANGUAGE DeriveDataTypeable #-}
module Process where
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
              | DummyVar
              | ArrayVar (Integer, Map.Map Integer Variable)
              | Partial [String] [Variable] Stmt
              deriving (Show, Typeable, Data)
type SymbolTable = Map.Map String Variable
type SymState = [SymbolTable]

nullSymState :: SymState
nullSymState = [Map.empty]

-- This is to wrap the dynamic scoping model
-- For this model, changes are local. It's easy to make global changes happen though(only this function requires change).

symUpdate :: SymState -> String -> Variable -> SymState
symUpdate (t:ts) name expr = (Map.insert name expr t):ts

symArrUpdate :: SymState -> String -> Integer -> Variable -> SymState
symArrUpdate (t:ts) name idx expr = (nt:ts) where
       nt = case (Map.lookup name t) of
            Nothing -> error $ "Variable not found: " ++ show name
            Just (ArrayVar (len, content)) ->
                if ((idx >= 0) && (idx < len))
                then let updated = (ArrayVar (len, (Map.insert idx expr content))) in (Map.insert name updated t)
                else error $ "Illegal subscription " ++ show idx ++ " in " ++ show name ++ "[" ++ show len ++ "]"
            otherwise -> error $ "Variable is not an array: " ++ show name


symLookup :: SymState -> String -> Maybe Variable
symLookup [] _ = Nothing
symLookup (s:ss) name = case Map.lookup name s of
                        Nothing -> symLookup ss name
                        Just v -> Just v

symArrLookup :: SymState -> String -> Integer -> Maybe Variable
symArrLookup state name idx = case symLookup state name of
                               Nothing -> error $ "Variable not found: " ++ show name
                               Just (ArrayVar (len, content)) -> if ((idx >= 0) && (idx < len)) then (Map.lookup idx content)
                                                                else error $ "Illegal subscription " ++ show idx ++ " in " ++ show name ++ "[" ++ show len ++ "]"
                               otherwise -> error $ "Variable is not an array: " ++ show name

-- Functionality for function calls.
-- Since "return" itself is a reserved key, we use it to store the actual return value of the function

-- Before a function call, return the symbol table containing its parameters. The old state is a parameter to fetch function list.
enterBlock :: SymState -> [String] -> [Variable] -> SymState
enterBlock state names vars = (nt:state) where nt = if (length names) == (length vars) then Map.fromList (zip names vars)
                                                          else error $ "Parameter length mismatch: " ++ show (length names) ++ " parameters and provided " ++ show (length vars)

leaveBlock :: SymState -> SymState
leaveBlock (s:ss) = ss

-- Is a return value assigned at the top level?
isRet :: SymState -> Bool
isRet (t:ts) = Map.member "return" t


-- Helper function to bind additional parameters
bindVar :: Variable -> [Variable] -> Variable
bindVar f params = case f of
    (Partial vars oldparam stmt) -> if (length vars < (length oldparam + length params)) then
                                        error $ show(f) ++ " receives too many parameters: " ++ show(params)
                                    else (Partial vars (oldparam ++ params) stmt)
    otherwise -> error $ show(f) ++ " is NOT a partial to bind parameters: " ++ show(params)

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
                (IntVar l) -> put (symUpdate symbolTable arrayName (ArrayVar (l, Map.fromList [])));
                otherwise -> error $ "incompatible type for array length: " ++ show (toConstr length)
        }
    (ArrayAssign arrayName expr1 expr2) ->
        do{
            symbolTable <- get;
            index <- evalExpr expr1;
            val <- evalExpr expr2;
            case index of
                (IntVar l) -> put (symArrUpdate symbolTable arrayName l val);
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
                (IntVar x) -> case symArrLookup symbolTable arrayName x of
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
    (Call func params) ->
        do {
            sym <- get;
            f <- evalExpr func;
            p <- evalExprList params;
            evalPartial (bindVar f p)
        }
    (Function vars stmt) -> return (Partial vars [] stmt)
    (Let varName varExpr expr) ->
        do {
            sym <- get;
            value <- evalExpr varExpr;
            put (enterBlock sym [varName] [value]);
            ret <- evalExpr expr;
            sym <- get;
            put (leaveBlock sym);
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


evalPartial :: Variable -> State SymState Variable
evalPartial f = case f of
                (Partial var param stmt) ->
                    if (length var == length param) || (head var == "") then
                        do{
                            old_sym <- get;
                            if (length var == length param) then put (enterBlock old_sym var param)
                                                            else put (enterBlock old_sym [] []);
                            evalStmt stmt;
                            new_sym <- get;
                            put (leaveBlock new_sym);
                            if isRet new_sym then return $ fromJust $ symLookup new_sym "return"
                                             else return (error $ "No return statement executing " ++ show(f))
                        }
                    else return f
                otherwise -> error $ "Internal Error - " ++ show(f) ++ " is not a partial."

evalProg :: ProgDecl -> Variable
evalProg (Program stmt) = evalState (evalExpr (Call (Var "main") [])) state
                                where state = execState (evalStmt stmt) nullSymState

runProg :: String -> Variable
runProg str = evalProg $ parseProgramStr str

runStmt :: String -> SymState
runStmt str = execState (evalStmt $ parseString str) nullSymState


-- Some cases for testing new implementation of array, the let clause, first order functions
test_ret = "(define (main) (begin (return 10)))" -- Outputs 10
test_call = "(define (test x y z) (begin (return (let q (+ x y) (+ z q))) )) (define (main) (return (test 5 10 15) ) )" -- Outputs 30
test_earlyhalt = "(define (main) (begin (set! x 10) (while (> x 0) (begin (set! x (- x 1)) (return x) ) ) ) )" -- Outputs 9 instead of 0
test_assign = "(define (main) (begin (make-vector a 10) (vector-set! a 5 10) (return (vector-ref a 5))) )" -- Outputs 10
test_miss = "(define (main) (begin (make-vector a 10) (vector-set! a 5 10) (return (vector-ref a 6))) )" -- Reports no initialization
test_bound = "(define (main) (begin (make-vector a 10) (vector-set! a 10 10)) )" -- Reports out of bound
test_missret = "(define (main) (set! x 10) )" -- Reports no return value
test_shadowing = "(define (main) (begin (set! x 10) (set! y (let x 15 x)) (return y)))" -- Outputs 15
test_recursive = "(define (add x) (if (= x 0) (return 100) (return (+ x (add (- x 1)))))) (define (main) (return (add 100)))" -- Outputs 5150
test_subarray = "(define (main) (begin (make-vector a 4) (make-vector b 4) (vector-set! a 0 b) (vector-set! b 0 1) (set! c (vector-ref a 0))" ++
                "(return (vector-ref c 0)) ))" -- Reports uninitialized value, which is actually the correct behaviour.

-- Cases for anonymous function, and passing anonymous functions as parameters
test_lambda_base = "(set! x (lambda d (+ d 5))) (define (main) (return (x 10))))"
test_multi_lambda = "(set! x (lambda2 (q w e) (+ q (+ w e)))) (define (main) (return (x 42 53 53)))"
test_passing_partial = "(set! x (lambda2 (q w e) (+ q (+ w e)))) (set! y (lambda d (+ (d 10) (d 15)))) (define (main) (return (y (x -10 -15)))))"


-- Cases for dynamic scoping and zero parameter functions
test_scoping_1 = "(define (f1) (begin (set! a 10) (return (f2)))) (define (f2) (return a)) (set! main f1)"
test_scoping_2 = "(define (f1 x) (begin (set! a x) (return 0))) (set! tmp (lambda x (f1 x))) (define (main) (return (+ (tmp 5) a)))"
test_scoping_3 = "(define (main) (return (+ a b))) (set! a 5) (set! b 5) "