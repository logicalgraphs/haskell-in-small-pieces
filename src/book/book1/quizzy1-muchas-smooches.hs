main :: IO ()
main = getLine >>= 
       putStrLn . ("Hello, " ++)
                . (++ "! I love you forever and want to give you muchas smooches!")
