module Main 
    where

import System.IO
import PTriangle (pTriangle, toRowMajor) 

main :: IO ()
main = do
    putStrLn "\nEnter triangle depth: "
    eof <- isEOF
    if not eof
        then do            
            inpStr <- getLine
            n <- readIO inpStr :: IO Int

            case pTriangle n of 
                Right result -> do
                    putStrLn $ toRowMajor result

                Left err -> do
                    putStrLn $ "ERROR: " ++ err
            main
        else putStr ""
