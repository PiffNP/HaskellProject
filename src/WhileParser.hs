{-# LANGUAGE DeriveDataTypeable #-}
module WhileParser where
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
identifier = Token.identifier lexer
reserved   = Token.reserved   lexer
reservedOp = Token.reservedOp lexer
parens     = Token.parens     lexer
integer    = Token.integer    lexer
whiteSpace = Token.whiteSpace lexer
charLiteral = Token.charLiteral lexer
stringLiteral = Token.stringLiteral lexer 

--{-
-- In case that TA insists that the parser can
-- not recognize exponent.
lexeme     = Token.lexeme     lexer
float = lexeme floating <?> "float"
floating = do{
    f <- lexeme sign;
    n <- many1 digit;
    char '.';
    fraction <- many1 digit;
    case reads (n ++ "." ++ fraction) of
        [(x, "")] -> return (f x)
        _         -> Prim.parserZero
}
sign = (char '-' >> return negate)
    <|> (char '+' >> return id)
    <|> return id
--}
whileParser :: Parser Stmt
whileParser = whiteSpace >> statement

funcStmt :: Parser Stmt
funcStmt = try (parens funcDeclStmt)
        <|> try (parens arrayDefStmt)
        <|> try (parens arrayAssignStmt)
        <|> try (parens assignStmt)

statement :: Parser Stmt
statement =  try (parens stmtList)
         <|> try (parens assignStmt)
         <|> skipStmt
         <|> try (parens ifStmt)
         <|> try (parens whileStmt)
         <|> try (parens arrayDefStmt)
         <|> try (parens arrayAssignStmt)
         <|> try (parens returnStmt)
         <|> try (parens funcDeclStmt)

stmtList :: Parser Stmt
stmtList = 
  do reserved "begin"
     stmtList <- many statement
     return (StmtList stmtList)
assignStmt :: Parser Stmt
assignStmt =
  do reserved "set!"
     var <- identifier
     expr <- expression
     return $ Assign var expr
skipStmt :: Parser Stmt
skipStmt = reserved "skip" >> return Skip
ifStmt :: Parser Stmt
ifStmt =
  do reserved "if"
     cond  <- expression
     stmt1 <- statement
     stmt2 <- statement
     return $ If cond stmt1 stmt2
whileStmt :: Parser Stmt
whileStmt =
  do reserved "while"
     cond <- expression
     stmt <- statement
     return $ While cond stmt
arrayDefStmt :: Parser Stmt
arrayDefStmt =
  do reserved "make-vector"
     var <- identifier
     expr <- expression
     return $ ArrayDef var expr
arrayAssignStmt :: Parser Stmt
arrayAssignStmt =
  do reserved "vector-set!"
     var <- identifier
     indexExpr <- expression
     valExpr <- expression
     return $ ArrayAssign var indexExpr valExpr
returnStmt :: Parser Stmt
returnStmt =
  do reserved "return"
     expr <- expression
     return $ Return expr
funcDeclStmt :: Parser Stmt
funcDeclStmt = do reserved "define";
                   cls <- parens $ many identifier;
                   stmts <- statement;
                   -- The Dummy Variable Hack
                   if (length cls) > 1 then return $ Assign (head cls) (Function (tail cls) stmts)
                                       else return $ Assign (head cls) (Function [""] stmts)

expression :: Parser Expr
expression = constExpr
          <|> liftM Var identifier
          <|> try (parens makePairExpr)
          <|> try (parens takeFstExpr)
          <|> try (parens takeSndExpr)         
          <|> try (parens arrayEntryExpr)
          <|> try (parens aExpr)
          <|> try (parens bExpr)
          <|> try (parens rExpr)
          <|> try (parens letExpr)
          <|> try (parens callExpr)
          <|> try (parens lambdaExpr)

constExpr :: Parser Expr
constExpr = try (liftM DoubleLit float)
         <|> try (liftM IntLit integer)
         <|> (reserved "True"  >> return (BoolLit True ))
         <|> (reserved "False" >> return (BoolLit False))
         <|> liftM CharLit charLiteral
         <|> liftM StringLit stringLiteral
         <|> (reserved "nil" >> return NilList)
makePairExpr :: Parser Expr
makePairExpr = 
  do reserved "cons"
     expr1 <- expression
     expr2 <- expression
     return $ Pair expr1 expr2
takeFstExpr :: Parser Expr
takeFstExpr =
  do reserved "car"
     expr <- expression
     return $ PairFst expr
takeSndExpr :: Parser Expr
takeSndExpr =
  do reserved "cdr"
     expr <- expression
     return $ PairSnd expr
arrayEntryExpr :: Parser Expr
arrayEntryExpr =
  do reserved "vector-ref"
     var <- identifier
     index <- expression
     return $ ArrayEntry var index
aExpr :: Parser Expr
aExpr =
  do op <- aBinOp
     expr1 <- expression
     expr2 <- expression
     return $ ABinary op expr1 expr2
aBinOp :: Parser ABinOp
aBinOp = (reservedOp "+" >> return Add)
   <|> (reservedOp "-" >> return Subtract)
   <|> (reservedOp "*" >> return Multiply)
   <|> (reservedOp "/" >> return Divide)
bExpr :: Parser Expr
bExpr =  do {reserved "not"; expr <- expression; return (Not expr)}
     <|> do op <- bBinOp
            expr1 <- expression
            expr2 <- expression
            return $ BBinary op expr1 expr2
bBinOp :: Parser BBinOp
bBinOp = (reserved "and" >> return And)
   <|> (reserved "or" >> return Or)
rExpr :: Parser Expr
rExpr =
  do op <- rBinOp
     expr1 <- expression
     expr2 <- expression
     return $ RBinary op expr1 expr2
rBinOp :: Parser RBinOp
rBinOp = (reservedOp "<" >> return WhileParser.LT)
      <|> (reservedOp "<=" >> return WhileParser.LE)
      <|> (reservedOp "=" >> return WhileParser.EQ)
      <|> (reservedOp ">=" >> return WhileParser.GE)
      <|> (reservedOp ">" >> return WhileParser.GT)

callExpr :: Parser Expr
callExpr =
  do func <- expression
     params <- many expression
     return $ Call func params

lambdaExpr :: Parser Expr
lambdaExpr =  try(do reserved "lambda"
                     var <- identifier
                     expr <- expression
                     return $ Function [var] $ Return expr)
          <|> try(do reserved "lambda"
                     vlist <- parens (many identifier)
                     expr <- expression
                     return $ Function vlist $ Return expr)

letExpr :: Parser Expr
letExpr =
  do reserved "let"
     varName <- identifier
     varBind <- expression
     bindExpr <- expression
     return $ Let varName varBind bindExpr

programDecl :: Parser ProgDecl
programDecl = do whiteSpace
                 stmts <- many funcStmt
                 whiteSpace
                 eof
                 return $ Program $ StmtList stmts

parseString :: String -> Stmt
parseString str =
    case parse whileParser "" str of
        Left e  -> error $ show e
        Right r -> r

parseProgramStr :: String -> ProgDecl
parseProgramStr str =
    case parse programDecl "" str of
        Left e -> error $ show e
        Right r -> r

