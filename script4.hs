--Myungsoo Son 20417585
--Dharmendra a/l Saravanakumar 20321437
import Data.Char

{--1. Define a function chomp that selects a run of repeated characters from the start of a string
, with the run being as long as possible--}
chomp :: String -> String
chomp "" = ""
chomp (x:xs) = takeWhile (==x) (x:xs)

{--2. Using chomp, define a function munch that selects a run of repeated characters from the start of a string
, with the run comprising at most nine characters--}
munch :: String -> String
munch = take 9 . chomp

{--3. Using munch, define a function runs that splits a string into a list of runs of repeated characters
, with each run comprising at most nine characters--} 
runs :: String -> [String]
runs "" = []
runs xs = ys : runs (drop (length ys) xs)
          where ys = munch xs

{--4. Using runs, define a function encode that transfroms a string into a list of pairs comprising the character from each
run together with its number of repetitions--}
encode :: String -> [(Char, Int)]
encode "" = []
encode xs = [(head x, length x) | x<- runs xs]

{--5. Define a function flatten that flattens a list of pairs of characters and digits to a string--}
flatten :: [(Char, Int)] -> String
flatten [] = ""
flatten ((x, y):xs) = [x] ++ [intToDigit y] ++ flatten xs

{--6. Using encode and flatten, define a function compress that compresses a string using run-length encoding--}
compress :: String -> String
compress = flatten . encode

{--7. Define a function decode that performs the inverse function to encode--}
decode :: [(Char, Int)] -> String
decode [] = ""
decode ((x, y):xs) = replicate y x ++ decode xs

{--8. Define a function expand that performs the inverse function to flatten--}
expand :: String -> [(Char, Int)]
expand "" = []
expand (x:y:xs) = (x, digitToInt y) : expand xs

{--9. Using decode and expand, define a function that performs the inverse function to compress--}
decompress :: String -> String
decompress = decode . expand