{-# LANGUAGE ViewPatterns #-}

import Control.Arrow ((&&&), second, (>>>))
import Control.Monad (join)

import Data.Either (partitionEithers, isRight, lefts)
import Data.List (sort, group)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import System.Environment (getArgs)

data Realm = CV | SD
   deriving (Eq, Ord, Show, Read)

data Hero =
   Hero { cost           :: Double,
          realm          :: Realm,
          idx, rarity    :: Integer,
          class1, class2 :: String,
          level, gen     :: Integer,
          trade          :: String,
          classScore     :: Double,
          growthScore    :: Double }
      deriving (Eq, Ord, Show)

data Line = Line Integer
   deriving (Eq, Ord, Show)

data LineError = Error Line String
   deriving (Eq, Ord, Show)

main :: IO ()
main = getArgs >>= mapM_ heroOut

heroOut :: FilePath -> IO ()
heroOut f = putStrLn ("Heroes in " ++ f ++ ":\n") >> heroM f >>=
            uncurry (resultsAndErrors f)

heroM :: FilePath -> IO ([Hero], [LineError])
heroM f = partitionEithers . join . zipWith heroic [3..] . drop 3 . lines
          <$> readFile f

resultsAndErrors :: FilePath -> [Hero] -> [LineError] -> IO ()
resultsAndErrors f heroes errs = results f heroes >> nl >> errors f errs

nl :: IO ()
nl = putStrLn ""

results :: FilePath -> [Hero] -> IO ()
results f heroes = mapM_ print heroes

errors :: FilePath -> [LineError] -> IO ()
errors _ [] = return ()
errors f lines =
   putStrLn ("Didn't parse following lines in " ++ f ++ ":\n") >>
   mapM_ print lines

csv :: String -> [String]
csv = split ','

-- https://stackoverflow.com/questions/4978578/how-to-split-a-string-in-haskell
-- Frank Meisschaert answer

split :: Char -> String -> [String]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s

-- The hero ontology as a structured (parseable) language

type ParseS a = Either a LineError
type ParseH = ParseS Hero

heroic :: Integer -> String -> [ParseH]
heroic x (csv -> elts) = parseHero x elts

data Scanned = Idx Integer
             | Num Double
             | Str String
             | Relm Realm
             | TF Bool
   deriving (Eq, Ord, Show)

type Scan = ParseS Scanned

parseHero :: Integer -> [String] -> [ParseH]
parseHero x (cost:relm:idx:rar:cls1:cls2:_:lvl:_:gen:_:_:_:trad:_:_:_:cs:_:gs:_) =
   let ls = Left . Str
       line = [parseNum x cost, parseRealm x relm, parseInt x idx, 
               parseInt x rar, ls cls1, ls cls2, parseInt x lvl,
               parseInt x gen, ls trad, parseNum x cs, parseNum x gs]
   in  uncurry (ph' x) (partitionEithers line)
parseHero x line = [Right (Error (Line x) ("Wrong CSV length: " ++ show line))]

ph' :: Integer -> [Scanned] -> [LineError] -> [ParseH]
ph' _ _ errs@(_:_) = map Right errs
ph' _ [Num cost, Relm r, Idx ix, Idx rar, Str cls1, Str cls2,
       Idx lvl, Idx gen, Str trad, Num cs, Num gs] [] =
   [Left $ Hero cost r ix rar cls1 cls2 lvl gen trad cs gs]
ph' x scan [] = [Right $ Error (Line x) ("Fandango on core: " ++ show scan)]

type ScanF = Integer -> String -> Scan

parseNum, parseRealm, parseInt, parseIntBool :: ScanF
parseNum x = parseT x Num "number"
parseRealm x = parseT x Relm "realm"
parseInt x = parseT x Idx "int"
parseIntBool x = eitherBool x . parseInt x

eitherBool :: Integer -> Scan -> Scan
eitherBool _ r@(Right _) = r
eitherBool _ (Left (Idx 0)) = Left (TF False)
eitherBool _ (Left (Idx 1)) = Left (TF True)
eitherBool x (Left (Idx n)) =
   Right (Error (Line x) ("Could not parse bool from " ++ show n))

parseT :: Read a => Integer -> (a -> Scanned) -> String -> String -> Scan
parseT _ f _ (reads -> [(a, "")]) = Left (f a)
parseT x _ t n = Right (Error (Line x) ("Parse to " ++ t ++ ": '" ++ n ++ "'"))

{--
>>> heroes <- heroM "heroes-serendale.csv" 
>>> let (hs, errs) = heroes
>>> (length hs, length errs)
(3675,0)

Does this bear out?

$ wc heroes-serendale.csv 
    3677    7357  507693 heroes-serendale.csv

Yeup.
--}

-- ANALYTICS --------------------------------------------------------------

type Distribution a = Map a Int

count :: (Eq b, Ord b) => (a -> b) -> [a] -> Distribution b
count f = Map.fromList . map cnt . group . sort . map f
   where cnt = head &&& length

data Percentage = P Double
   deriving (Eq, Ord)

instance Show Percentage where
   show (P p) = (uncurry (++) . second ((++ "%") . take 3)) 
                (break (=='.') (show (100 * p)))

type Frequency a = Map a Percentage

