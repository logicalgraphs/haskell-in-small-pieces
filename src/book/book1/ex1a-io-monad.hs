main :: IO ()
main = getLine >>= 
       putStrLn . ("Hello, " ++)
