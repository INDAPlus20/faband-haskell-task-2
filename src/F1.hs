module F1 where
import Data.Char

repeat_fun :: Integer -> (a -> a) -> (a -> a)
-- Vid den givna parametern 0 så returneras ett godtyckligt x
repeat_fun 0 f x = x
repeat_fun n f x = repeat_fun (n-1) f (f x)

fib :: Integer -> Integer

fib n = fst (repeat_fun n (\(x,y) -> (y, x+y)) (0, 1))

rovarsprak :: String -> String
rovarsprak s = concatMap (\c -> if isConsonant c then [c, 'o', c] else [c]) s
-- Gör en funktion från tecken `Char` till `Bool` som kollar fall det är en konsonant
isConsonant :: Char -> Bool
isConsonant c = c `elem` "bcdfghjklmnpqrstvwxz"

karpsravor :: String -> String
karpsravor [] = []
karpsravor (c:s) = if isConsonant c then c:karpsravor (drop 2 s) else c:karpsravor s 
-- Eftersom vi enbart får en sträng i pirat språk så behöver vi inte felhantera konsonanter
-- Vi vet att alla konsonanter har 2 'onödiga' bokstäver i följd
isDivider d = not (isAlpha d) -- Ser till så att 'd' inte är en bokstav

data Average a = Average { averageSum :: a
                         , averageCount :: Integer
                         }
               deriving (Show, Eq, Ord)

instance Num a => Semigroup (Average a) where
  Average sum1 count1 <> Average sum2 count2 = Average (sum1 + sum2) (count1 + count2)

instance Num a => Monoid (Average a) where
  mempty = Average 0 0

toAverage :: a -> Average a
toAverage x = Average x 1

fromAverage :: Integral a => Average a -> Double
fromAverage (Average sum count) = (fromIntegral sum) / (fromInteger count)

splitDivider :: String -> [String]
splitDivider s = case dropWhile isDivider s of
            "" -> []
            s' -> w : splitDivider s''
                where
                  (w, s'') = break isDivider s'

medellangd :: String -> Double
medellangd = fromAverage . foldMap (toAverage . length) . splitDivider

-- A function that splits a list into two with every other element in the "first" split

halve :: [a] -> ([a], [a])
halve [] = ([], [])
halve [x] = ([x], [])
halve (x:y:xs) =
  let (ys, zs) = halve xs in
    (x:ys , y:zs)

skyffla :: [a] -> [a]
skyffla [] = []
skyffla [x] = [x]
skyffla (x:y:xs) =
  let (ys, zs) = halve xs in
    (x:ys)++skyffla(y:zs)