-- Exercise 2:
--
-- Change the program so it performs a simple arithmetic operation on the 
-- two arguments and prints out the result. You can use read to convert a 
-- string to a number, and show to convert a number back into a string. 
-- Play around with different operations.

module Main where
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case length args of
        3 -> let [strNum1, op, strNum2] = take 3 args 
                 n1 = read strNum1 :: Double
                 n2 = read strNum2 :: Double
             in case op of 
                "+" -> putStrLn (show (n1 + n2))
                "-" -> putStrLn (show (n1 - n2))
                "*" -> putStrLn (show (n1 * n2))
                "/" -> putStrLn (show (n1 / n2))
        -- this could probably be even more overengineered, 
        -- but I stopped enjoying it around here
        2 -> let sn1 = args !! 0
                 sn2 = args !! 1
                 n1  = read sn1 :: Double
                 n2  = read sn2 :: Double
                 res = n1 + n2
             in putStrLn $ sn1 ++ " + " ++ sn2 ++ " = " ++ (show res)
        _ -> putStrLn "Please provide two (numeric) or three (num1, num2, operator) arguments."
