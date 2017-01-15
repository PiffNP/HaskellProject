{-# LANGUAGE DeriveDataTypeable #-}
module Spec where
import Data.Data
import System.IO
import Control.Monad
import Text.Parsec (Parsec, runP)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import qualified Text.Parsec.Prim as Prim

data Stmt = StmtList [Stmt]
          | Assign String Expr
          | Skip          
          | If Expr Stmt Stmt 
          | While Expr Stmt
          | ArrayDef String Expr
          | ArrayAssign String Expr Expr
          | Return Expr
            deriving (Data, Typeable, Eq)
data Expr = BoolLit Bool 
          | IntLit Integer
          | DoubleLit Double
          | CharLit Char
          | StringLit String
          | NilList
          | Var String
          | ArrayEntry String Expr
          | ABinary ABinOp Expr Expr
          | Not Expr
          | BBinary BBinOp Expr Expr
          | RBinary RBinOp Expr Expr
          | Pair Expr Expr
          | PairFst Expr
          | PairSnd Expr
          | Call Expr [Expr]
          | Function [String] Stmt
          | Let String Expr Expr
            deriving (Show, Data, Typeable, Eq)
data ProgDecl = Program Stmt
data BBinOp = And | Or deriving (Show, Data, Typeable, Eq)
data RBinOp = EQ | GE | LE | GT | LT deriving (Show, Data, Typeable, Eq)
data ABinOp = Add | Subtract | Multiply | Divide deriving (Show, Data, Typeable, Eq)

-- pretty-printer
showStmtList :: Int -> [Stmt] -> String
showStmtList _ [] = ""
showStmtList t (x:xs) = (showStmt t x) ++ (showStmtList t xs)
showStmt :: Int -> Stmt -> String
showStmt t stmt = [' ' | x <- [1..t]] ++ case stmt of
    (StmtList xs) -> "StatementList [\n" ++ showStmtList (t+2) xs ++ [' ' | x <- [1..t]] ++ "]\n"
    (Assign str expr) -> case expr of
        (Function s stmt) -> "Assign " ++ show str ++ " Function " ++ show s ++  " {\n" ++ showStmt (t+2) stmt ++ [' ' | x <- [1..t]] ++ "}\n"
        _ -> "Assign " ++ show str ++ " " ++ show expr ++ [' ' | x <- [1..t]] ++ "\n"
    (Skip) -> "Skip\n"
    (If expr a b) -> "If " ++ show expr ++ ":\n" ++ showStmt (t+2) a ++ [' ' | x <- [1..t]] ++ "Else:\n" ++ showStmt (t+2) b
    (While expr a) -> "While " ++ show expr ++ ":\n" ++ showStmt (t+2) a
    (ArrayDef str expr) -> "ArrayDef " ++ show str ++ " " ++ show expr ++ "\n"
    (ArrayAssign str expr1 expr2) -> "ArrayAssign " ++ show str ++ " " ++ show expr1 ++ " " ++ show expr2 ++ "\n"
    (Return expr) -> "Return " ++ show expr ++ "\n"
instance Show Stmt where
    show a = showStmt 0 a
instance Show ProgDecl where
    show (Program stmt) = "Program {\n" ++ showStmt 2 stmt ++ "}"

lexer = Token.makeTokenParser emptyDef{ 
        Token.commentStart    = "/*",
        Token.commentEnd      = "*/",
        Token.commentLine     = "//",
        Token.identStart      = letter,
        Token.identLetter     = alphaNum,
        Token.reservedNames   = [ "if", "while", "begin", "do",
                                  "set!", "skip", "True", "False",
                                  "not", "and", "or", "cons", "car",
                                  "cdr", "vector-ref", "make-vector",
                                  "vector-set!", "nil", "return", "function",
                                  "let", "define", "lambda"
                                ],
        Token.reservedOpNames = ["+", "-", "*", "/", "<", "=",
                                 "<", "<=", ">", ">=", "!"
                                ],
        Token.caseSensitive = True
    }