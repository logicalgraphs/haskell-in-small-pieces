import Data.Either (partitionEithers)
import Data.Maybe (listToMaybe)

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)

data Coin = Coin { cmc_id :: Integer, name :: String }
   deriving (Eq, Ord, Show)

data Line = Line Integer
   deriving (Eq, Ord, Show)

data LineError = Error Line String
   deriving (Eq, Ord, Show)

main :: IO ()
main = getArgs >>= mapM_ coinOut

coinOut :: FilePath -> IO ()
coinOut f = putStrLn ("Coins in " ++ f ++ ":\n") >> coinM f >>=
            uncurry (resultsAndErrors f)

coinM :: FilePath -> IO ([Coin], [LineError])
coinM f = partitionEithers . zipWith encoin [3..] . drop 3 . lines
          <$> readFile f

resultsAndErrors :: FilePath -> [Coin] -> [LineError] -> IO ()
resultsAndErrors f coins errs = results f coins >> nl >> errors f errs

nl :: IO ()
nl = putStrLn ""

results :: FilePath -> [Coin] -> IO ()
results f coins = mapM_ print coins

putErrLn :: String -> IO ()
putErrLn = hPutStrLn stderr

printErr :: Show a => a -> IO ()
printErr = putErrLn . show

errors :: FilePath -> [LineError] -> IO ()
errors _ [] = return ()
errors f lines =
   putErrLn ("Didn't parse following lines in " ++ f ++ ":\n") >>
   mapM_ printErr lines

csv :: String -> [String]
csv = split ','

-- https://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell
-- Frank Meisschaert answer

split :: Char -> String -> [String]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s

encoin :: Integer -> String -> Either Coin LineError
encoin x = encoin' x . csv

encoin' :: Integer -> [String] -> Either Coin LineError
encoin' x (_:cmc_id:name:_) =
   maybe (Right (Error (Line x) ("Couldn't parse " ++ cmc_id ++ " as cmc_id.")))
         (Left . flip Coin name . fst) (listToMaybe $ reads cmc_id)
encoin' x line = Right $ Error (Line x) (show line)
