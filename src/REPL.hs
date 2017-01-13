import Process
import WhileParser
import qualified Data.Map.Strict as Map
import Text.Parsec (Parsec, runP)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Control.Monad.State.Lazy
import Control.Monad
import System.IO

showSymState :: SymState -> String
showSymState (s, _, fl) = "Active variables: " ++ (show s) ++ "Functions: " ++ (show (Map.keys fl))

addFunc :: SymState -> FuncDecl -> SymState
addFunc (s, t, fl) f = (s, t, (Map.insert (getFuncName f) f fl))

type IOState = (SymState, Int, Stmt)

runCycle :: IOState -> IO ()
runCycle (state, lineno, laststmt) = do
                                        str <- getLine
                                        let command = (drop 3 str)
                                        case (take 2 str) of
                                            ":f" -> case parse functionDecl "" command of
                                                        Left e -> do {
                                                            putStrLn $  "Parsing Error: " ++ show(e);
                                                            putStr $ ((show lineno) ++ "> ");
                                                            runCycle (state, lineno + 1, laststmt)
                                                        }
                                                        Right r -> do {
                                                            putStrLn $  "Function will be added to current state";
                                                            putStr $ ((show lineno) ++ "> ");
                                                            runCycle (addFunc state r, lineno + 1, laststmt)
                                                        }
                                            ":i" -> case parse whileParser "" command of
                                                    Left e -> do {
                                                            putStrLn $  "Parsing Error: " ++ show(e);
                                                            putStr $ ((show lineno) ++ "> ");
                                                            runCycle (state, lineno + 1, laststmt)
                                                        }
                                                    Right r -> let newstate = execState (evalStmt r) state in
                                                                            do {
                                                                                putStrLn (showSymState newstate);
                                                                                putStr $ ((show lineno) ++ "> ");
                                                                                runCycle (newstate, lineno + 1, r)
                                                                            }
                                            ":p" -> case parse expression "" command of
                                                        Left e -> do {
                                                            putStrLn $  "Parsing Error: " ++ show(e);
                                                            putStr $ ((show lineno) ++ "> ");
                                                            runCycle (state, lineno + 1, laststmt)
                                                        }
                                                        Right r -> let result = evalState (evalExpr r) state in
                                                                        do {
                                                                            putStrLn $ "Evaluation: " ++ (show result);
                                                                            putStr $ ((show lineno) ++ "> ");
                                                                            runCycle (state, lineno + 1, laststmt)
                                                                        }
                                            ":t" -> do {
                                                            putStrLn $ show laststmt;
                                                            putStr $ ((show lineno) ++ "> ");
                                                            runCycle (state, lineno + 1, laststmt)
                                                        }
                                            ":q" -> return ()
                                            otherwise -> do {
                                                            putStrLn $ "Unknown prefix - [" ++ (take 3 str) ++ "]";
                                                            putStr $ ((show lineno) ++ "> ");
                                                            runCycle (state, lineno + 1, laststmt)
                                                        }



repl :: IO ()
repl = do
            putStrLn $  "You are now at REPL Mode. :f to define function, :i to execute code, :p to print variable to screen" ++
                     ":q to quit, :t to print AST of last :i action."
            putStr "> "
            runCycle (nullSymState, 1, Skip)

