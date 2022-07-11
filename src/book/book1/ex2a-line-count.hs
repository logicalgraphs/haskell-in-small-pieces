import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= mapM_ wc

wc :: FilePath -> IO ()
wc f = readFile f >>=
       putStrLn
              . ((f ++ " has ") ++) . (++ " lines.")
              . show . length . lines
