-- example pattern matching function mapping integral to string
sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe x = "Not between 1 and 5"
--    ^ pattern matching should always include a catch-all to prevent exceptions

-- Recursive pattern matching
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Pattern matching comprehensions
tupleSum :: (Num a) => [(a, a)] -> [a]
tupleSum x = [a+b | (a,b) <- x]

-- NOTE: `[1,2,3]` is just syntactic sugar for `1:2:3:[]`, where `:` is the cons operator
-- This allows us to do pattern matching, like the example below
head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x
-- NOTE: tail returns [a], as `_` only eats a single item if in front apparently.
tail' :: [a] -> [a]
tail' [] = error "Can't call tail on an empty list, dummy!"
tail' (_:xs) = xs
-- To match only the very last item, we need to make it recursive
last' [] = error "Can't call last on empty list, dummy!"
last' (x:[]) = x -- if only one item, return.
last' (_:xs) = last' xs -- if more than one item, recurse on tail

-- Another cool recursive implementation using pattern matching, length
length' :: (Num b) => [a] -> b
length' [] = 0 -- if empty, 0
length' (_:xs) = 1 + length' xs -- if non-empty, recurse on 1+length' tail

-- NOTE: this is essentially a reduce implementation, as we are accumulating
--       as we iterate over the list. We could define reduce via:
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = (f x) : (map' f xs)
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs) | f x       = x : recurse
                 | otherwise = recurse
                 where recurse = filter' f xs
-- See guards    ^ below
-- See where below ^

-- We can pattern match everything _and_ subsets via a@(b,c,...)
capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- Guards are boolean expressions that allow you to define function body by a boolean expression
-- Basically, a very readable if-else
bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
    | bmi <= 18.5 = "Underweight"
    | bmi <= 25.0 = "Normal"
    | bmi <= 30.0 = "Overweight"
    | otherwise   = "Obese"
-- ^ Default, like else

-- Guards can use multiple variables
max' :: (Ord a) => a -> a -> a
max' a b 
    | a > b     = a
    | otherwise = b

-- We can use where to avoid repeated calculations
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= 18.5 = "Underweight"
    | bmi <= 25.0 = "Normal"
    | bmi <= 30.0 = "Overweight"
    | otherwise   = "Obese"
    where bmi = weight / height ^ 2

-- Let expressions are like where, but hyperlocal and do not spread
--   across guards or other expressions