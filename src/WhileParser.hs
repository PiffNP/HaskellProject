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
import qualified Text.ParserCombinators.Parsec.Prim as Prim

data Stmt = StmtList [Stmt]
          | Assign String Expr
          | Skip          
          | If Expr Stmt Stmt 
          | While Expr Stmt
            deriving (Show)
data Expr = BoolConst Bool 
          | IntConst Integer
          | DoubleConst Double
          | Var String
          | ABinary ABinOp Expr Expr
          | Not Expr
          | BBinary BBinOp Expr Expr
          | RBinary RBinOp Expr Expr          
            deriving (Show)
data BBinOp = And | Or deriving (Show)
data RBinOp = EQ | GE | LE | GT | LT deriving (Show)
data ABinOp = Add | Subtract | Multiply | Divide deriving (Show)

lexer = Token.makeTokenParser emptyDef{ 
        Token.commentStart    = "/*",
        Token.commentEnd      = "*/",
        Token.commentLine     = "//",
        Token.identStart      = letter,
        Token.identLetter     = alphaNum,
        Token.reservedNames   = [ "if", "while", "begin", "do",
                                  "set!", "skip", "True", "False",
                                  "not", "and", "or"
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
float    = Token.float      lexer
{-
-- In case that TA insists that the parser can
-- not recognize exponent.
lexeme     = Token.lexeme     lexer
float = lexeme floating <?> "float"
floating = do{
    n <- many1 digit;
    char '.';
    fraction <- many1 digit;
    case reads (n ++ "." ++ fraction) of
        [(x, "")] -> return x
        _         -> Prim.parserZero
}
--}
whileParser :: Parser Stmt
whileParser = whiteSpace >> statement

statement :: Parser Stmt
statement =  try (parens stmtList)
         <|> try (parens assignStmt)
         <|> skipStmt
         <|> try (parens ifStmt)
         <|> try (parens whileStmt)
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
expression :: Parser Expr
expression = constExpr
          <|> liftM Var identifier
          <|> try (parens aExpr)
          <|> try (parens bExpr)
          <|> try (parens rExpr)
constExpr :: Parser Expr
constExpr = try (liftM DoubleConst float)
         <|> liftM IntConst integer
         <|> (reserved "True"  >> return (BoolConst True ))
         <|> (reserved "False" >> return (BoolConst False))
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
parseString :: String -> Stmt
parseString str =
    case parse whileParser "" str of
        Left e  -> error $ show e
        Right r -> r

testParser :: Parsec String Int RBinOp
testParser = whiteSpace >> testOp
test :: String -> RBinOp
test str =
    case runP testParser 0 "" str of
        Left e  -> error $ show e
        Right r -> r
testOp :: Parsec String Int RBinOp
testOp = (reservedOp "<" >> return WhileParser.LT)
      <|> (reservedOp "<=" >> return WhileParser.LE)
      <|> (reservedOp "=" >> return WhileParser.EQ)
      <|> (reservedOp ">=" >> return WhileParser.GE)
      <|> (reservedOp ">" >> return WhileParser.GT)
