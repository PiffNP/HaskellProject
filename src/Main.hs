module Main where
import REPL
import System.Environment (getArgs)
import System.IO

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
