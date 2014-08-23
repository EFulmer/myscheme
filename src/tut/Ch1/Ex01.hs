-- Exercise 1:
--
-- Change the program so it reads two arguments from the command line, and 
-- prints out a message using both of them

module Main where
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    if length args == 2 
    then putStrLn ("Hello, " ++ args !! 0 ++ ", and hello, " ++ args !! 1 
        ++ "!")
    else putStrLn "Sorry, but I need two arguments to work right."

