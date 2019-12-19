-- 1
-- determines if f is a tautology
-- take f is a logical formula with 2 variables
-- If all possible cases, f returns true. f is a tautology, return True
-- Else return False, not tautology 

twoTautology:: ((Bool,Bool) -> Bool) -> Bool
twoTautology f = if f(True, True) && f(True,False) && f(False, True) && f(False, False) then True else False

-- determines whether 2 logical functions of two variables are equivalent
twoEquiv::((Bool,Bool)->Bool)->((Bool,Bool)->Bool)->Bool
twoEquiv f g = if f(True, True) == g(True,True) && f(True,False) == g(True, False) && f(False, True) == f(False, True) && f(False, False)== g(False,False)
               then True else False

--2
-- showing Fermat's conjecture is false 
-- If this conjecture is true, it will go to infinite loop

-- isPrime is used to chekc if the number is a prime number
isPrime :: Integer -> Bool
isPrime n = case(filter (\x -> (n `mod` x) == 0)[2..(n-1)]) of
            [] -> True
            (_:_) -> False

-- call badFermat, return number which is no ttrue for Fermat's Conjecture
badFermat :: Integer
badFermat = bfHelper 1
  where
    ex n = 2^(2^n)+1
    bfHelper n = if isPrime (ex n)
                 then bfHelper (n+1)
                 else n

-- 3
-- Define type SF
data SF a = SS a
          | FF
   deriving (Show, Eq)

-- if input is odd, multiply by 3 and add 1
-- if input is even, divide by 2
collatz :: Int -> Int
collatz x = if (x `mod` 2 == 0) then (x `div` 2) else (3*x+1)

-- return a sequence of number that needs to applied collatz in order to get 1
-- if input is less than 1, return FF
-- otherwise, call collatzIndexHelper recursively to apply collatz functions until we get 1
collatzIndex :: Int -> SF [Int]
collatzIndex x = if (x < 1) then FF else SS (collatzIndexHelper x)
           where collatzIndexHelper n = if n==1 then [n] else (n:collatzIndexHelper(collatz n))


--4
-- fix e to be a very small number
e :: Double
e = exp (-36)

-- bisection is used to find approximate roots of continuous functions
-- It takes 1 function return double and a tuple, return type maybe
-- if the difference is very small, return nothing
-- using bisectionHelp to recursively determine middle number and apply method
-- if f a and f b are opposite sign, get the middle number and apply bisection method again
-- then apply the bisectionHelp with middle number and a or b
-- until we get the difference very small, less than e, return middle number
-- the middle number will be the roots

bisection::(Double->Double)->(Double,Double)->Maybe Double
bisection f (a,b)
       | abs (b-a)/2 < e = Nothing
       | a < b = bisectionHelp a b
       | otherwise = bisectionHelp b a
       where
           bisectionHelp x y
               | dif < e || f mid == 0.0 = Just mid
               | f x * f mid < 0.0 = bisectionHelp x mid
               | f mid * f y < 0.0 = bisectionHelp mid y
               | otherwise = Nothing
               where
                dif = (y-x)/2
                mid = (y+x)/2

-- 5
---- Bubble Sort
-- isSorted is used to check if the list is sorted in orders
-- return True if list is empty or has 1 element
-- otherwise compare the first 2 elements and recursively check the whole list
isSorted :: (Ord a) => (a -> a -> Bool) -> [a]-> Bool
isSorted p [] = True
isSorted p [x] = True
isSorted p (x:(y:ys)) = if (p x y) then isSorted p (y:ys) else False

-- bubble is used to swap elements if they are not in the right order
--  compare the first 2 elements and bubble them if they are not in the right order
-- if they are in the rigth order, check the next first 2 elements of remening list
-- else swap them and recursively call bubble
bubble :: (Ord a) =>(a -> a -> Bool) -> [a] -> [a]
bubble p [] = []
bubble p (x:[]) = x:[]
bubble p (x:(y:ys)) = if (p x y) then x:(bubble p (y:ys)) else y:(bubble p (x:ys))

-- bsort is used to sort a list in order
-- if the list is sorted in order, return the list, otherwise recursively apply bsort and bubble
bsort:: (Ord a) => (a -> a -> Bool) -> [a] -> [a]
bsort p xs = if (isSorted p xs)
               then xs
               else bsort p (bubble p xs)

--  qsort is used to sort the list into order
-- return empty list if an empty list is given
-- choose the first pivot point, sort everything not satify function to the left side
-- sort everything satify the function to th right side
-- and recursively call qsort until all elements are sorted
---- Quick Sort
qsort :: (Ord a) => (a -> a -> Bool) -> [a] -> [a]
qsort p [] = []
qsort p (x:xs) =
    let leftSort = qsort p (filter (not . p x) xs)
        rightSort = qsort p (filter (p x) xs)
    in  leftSort ++ [x] ++ rightSort

-- msortuses split function to slit list into smaller lists
-- wthen rearrange elements to the rigth order and merge the lists
-- Merge Sort
-- use split the list into 2 smaller lists
split :: [a] -> ([a],[a])
split = foldr (\a (as,bs) -> (bs, a:as))([],[])

-- return lists if the other list is empty
-- compare elements from 2 lists, put them in order and merge lists 
merge :: (Ord a) => (a -> a -> Bool) -> [a]->[a]->[a]
merge p as [] = as
merge p [] bs = bs
merge p (a:as) (b:bs) = if (p a b) then a : (merge p as (b:bs)) else b : (merge p (a:as) bs)

-- return empty list or lists has 1 element
-- split and merge lists after sorted
-- recursively call msort to split and merge them in order
msort :: (Ord a) => (a -> a -> Bool) -> [a]->[a]
msort p [] = []
msort p [a] = [a]
msort p as = case (split as) of
             (xs,ys) -> merge p (msort p xs)(msort p ys)

-- 9
-- Factoial take integer and returns its factorial
-- if input i s0, return 1
-- otherwise recursively call factorial function to calculate.
fac :: Integer -> Integer
fac 0 = 1
fac n = n * fac (n-1)
