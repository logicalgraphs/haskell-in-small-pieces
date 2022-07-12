import Data.Maybe (mapMaybe)
import System.Environment (getArgs)

data Coin = Coin { cmc_id :: Integer, name :: String }
   deriving (Eq, Ord, Show)

main :: IO ()
main = getArgs >>= mapM_ coinM

coinM :: FilePath -> IO ()
coinM f = putStrLn ("Coins in " ++ f ++ ":\n") >>
          readFile f >>= 
          mapM_ print . mapMaybe (encoin . csv) . drop 3 . lines

csv :: String -> [String]
csv = split ','

-- https://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell
-- Frank Meisschaert answer

split :: Char -> String -> [String]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s

encoin :: [String] -> Maybe Coin
encoin (_:cmc_id:name:_) = Just $ Coin (read cmc_id) name
encoin _ = Nothing
