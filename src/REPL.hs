import Process
import WhileParser
import qualified Data.Map.Strict as Map
import Text.Parsec (Parsec, runP)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Control.Monad.State.Lazy
import Control.Monad
import System.Environment (getArgs)
import System.IO

showSymState :: SymState -> String
showSymState = show


type IOState = (SymState, Int, Stmt)

runCycle :: IOState -> IO ()
runCycle (state, lineno, laststmt) = do
                                        str <- getLine
                                        let command = (drop 3 str)
                                        case (take 2 str) of
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


processLine :: Handle -> Handle -> Int -> String -> String -> IO ()
processLine inh ouh lineno prog mode =
       do isEof <- hIsEOF inh
          if isEof
              then do case mode of
                          ":i" -> hPutStr ouh $ show $ runProg prog;
                          ":t" -> hPutStr ouh $ show $ parseProgramStr prog;
                      hClose inh
                      hClose ouh
                      return ()
              else do str <- hGetLine inh
                      case parse whileParser "" str of
                          Left e -> do putStrLn $ "Parsing Error: line " ++ ((show lineno) ++ "> ") ++ show(e)
                                       processLine inh ouh (lineno + 1) (prog ++ " " ++ str) mode
                          Right r -> processLine inh ouh (lineno + 1) (prog ++ " " ++ str) mode


main :: IO ()
main = do
            args <- getArgs
            case args of
                ["-i", file1, "-o", file2] -> do inh <- openFile file1 ReadMode
                                                 ouh <- openFile file2 WriteMode
                                                 processLine inh ouh 1 "" ":i"
                ["-t", file1, "-o", file2] -> do inh <- openFile file1 ReadMode
                                                 ouh <- openFile file2 WriteMode
                                                 processLine inh ouh 1 "" ":t"
                ["-i", file1] -> do inh <- openFile file1 ReadMode
                                    processLine inh stdout 1 "" ":i"
                ["-t", file1] -> do inh <- openFile file1 ReadMode
                                    processLine inh stdout 1 "" ":t"
                ["-repl"] -> repl
                otherwise -> putStrLn "usage:\n  ki -repl\n  ki -i <file> [-o <file>]\n  ki -t <file> [-o <file>]"

repl :: IO ()
repl = do
            putStrLn $  "You are now at REPL Mode. :i to execute code, :p to print variable to screen " ++
                     ":q to quit, :t to print AST of last :i action."
            putStr "> "
            runCycle (nullSymState, 1, Skip)
