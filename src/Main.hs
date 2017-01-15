module Main where
import Process
import WhileParser
import REPL
import Text.ParserCombinators.Parsec
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
                      processLine inh ouh (lineno + 1) (prog ++ " " ++ str) mode