import Data.Either (partitionEithers)
import Data.Maybe (listToMaybe)
import System.Environment (getArgs)

data Coin = Coin { cmc_id :: Integer, name :: String }
   deriving (Eq, Ord, Show)

main :: IO ()
main = getArgs >>= mapM_ coinM

coinM :: FilePath -> IO ()
coinM f = readFile f >>= 
          (\(cs, es) -> results "coins.out" cs >> errors f es)
          . partitionEithers . map (encoin . csv) . drop 3 . lines >>
          putStrLn "Wrote results to 'coins.out'"

results :: FilePath -> [Coin] -> IO ()
results f = writeFile f . unlines . map show

errors :: FilePath -> [String] -> IO ()
errors _ [] = return ()
errors f lines =
   mapM_ putStrLn (("Didn't parse following lines in " ++ f ++ ":"):"":lines)

csv :: String -> [String]
csv = split ','

-- https://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell
-- Frank Meisschaert answer

split :: Char -> String -> [String]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s

encoin :: [String] -> Either Coin String
encoin (_:cmc_id:name:_) =
   maybe (Right ("Could not parse " ++ cmc_id ++ " as cmc_id."))
         (Left . flip Coin name . fst) (listToMaybe $ reads cmc_id)
encoin line = Right $ show line
