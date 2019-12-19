-- Minh Hang Chu 30074056
-- Fall 2019 CPSC 449
-- Exercise 1 - Starting exercise with Haskell


-- 01
-- The function takes 3 integers as inputs and return the average of 3 numbers as float type
-- Using fromIntegral to convert to type float. Take sum of three numbers then divide by 3.
avgThree:: Integer -> Integer -> Integer -> Float
avgThree a b c = fromIntegral((a + b + c)) / 3

-- 02
-- Write a function that find the maximum numbers and returns how many times that number occurs

-- fmax function is used to find the bigger number. The function takes 2 integers input and returns bigger number
-- if x is bigger or equal to y then return x, otherwise return y
fmax :: Integer -> Integer -> Integer
fmax x y | x >= y    = x
         | otherwise = y

-- ffilter is used to filter elements from the list given and return a list with elements satisfying the condition
-- if the list is empty, return empty list
-- if the list is not empty, take the first element and check condition
-- if the conditions are satisfied, add element into the output list and recursively call ffilter
-- otherwise, recursively call ffilter with the rest of the list
ffilter:: (a -> Bool) -> [a] -> [a]
ffilter p [] = []
ffilter p (a:as) = if (p a) then a:(ffilter p as)
                  else ffilter p as

--flength is used to get the length of the lists.
-- The function takes a list as input and return how many elements are in the list
-- If the list is empty, return 0
-- Take out the head of the list and +1, recursively call flength with the rest of the list
flength :: [a] -> Integer
flength [] = 0
flength (_:xs) = 1 + flength xs

-- The function takes 3 integers as inputs
-- using fmax to find the biggest number of 3 inputs and store in variable maxNum
-- using ffilter and flength functions in order to find number of maxNum
-- ffilter to filter the maximum in the list and take the length of that list
maxThree:: Integer -> Integer -> Integer -> (Integer,Integer)
maxThree a b c = let maxNum = fmax (fmax a b) c
                     numberOfMax = flength (ffilter (==maxNum) [a,b,c])
                     in (maxNum, numberOfMax)

-- 03
-- returns the largest number whose factorial is no greater than the given number

-- factorial function is used to calculate factorial of n using recursive
fac :: Integer -> Integer
fac 0 = 1
fac n = n * fac (n-1)

-- invFac is used to return largest number whose factorial is no greater than input
-- calling cFac to compare the factorial with the number given
-- if the number given is positive, showing error message
invFac:: Integer -> Integer
invFac x | x <= 0 = error "Positive number only"
         | otherwise = cFac 1 x

-- cFac is used to compare factorial of a number and a given number
-- if factorial of x is greater than y, return x-1
-- If factorial of x is equal to y, return x
-- otherwise, keep checking with x+1
cFac :: Integer -> Integer -> Integer
cFac x y | fac x >= y = x-1
         | fac x == y = x
         | otherwise = cFac (x+1) y

-- 04
-- the function takes 2 integers  as arguments and returns the greatest common divisors
-- if inputs are 0 0 GCD is not undefined
-- If inputs are smaller than 0, take absolute value
-- otherwise recursive call myGCD with second number and remainder (Euclid's Algorithm)
myGcd :: Integer -> Integer -> Integer
myGcd 0 0 = undefined
myGcd a 0 = abs a
myGcd a b | a <  0 = myGcd (abs a) b
          | b <  0 = myGcd a (abs b)
          | otherwise = myGcd b (a `rem` b)

-- 05
-- Write a function to calculate binomial coefficient
-- fproduct is used to calculate product of a given list
-- recursively calling function, multiply number with the rest of the list
fproduct :: Num a => [a] -> a
fproduct [] = 1
fproduct (x:xs) = x * fproduct (xs)

-- calculate binomial coefficient by using (fproduct of n to n-k+1) divide (fproduct from 1 to k)
-- check conditions. If not satisfied, show error messages
binom:: Integer -> Integer -> Integer
binom 0 0 = undefined
binom n k | k < 0 = error "Enter non negative numbers"
          | n < k = error "n should be greater or equal to k"
          | n < 1 = error "n should be greater or equal 1"
          | otherwise = fproduct [n-k+1..n] `div` fproduct [1..k]

--06
-- fappend is used to append to a list
fappend :: [a] -> [a] -> [a]
fappend [] ys = ys
fappend (x:xs) ys = x : (fappend xs ys)

-- fcopy is used to replicate a n times and returns a list of a with n elements
fcopy :: (Num i, Ord i) => i -> a -> [a]
fcopy n x
    | n <= 0    = []
    | otherwise = x:fcopy (n-1) x

-- fgrow is used to copy a string n times, and append them into a list
-- recursively call function for the rest of the string
-- if string is empty, return an empty String
fgrow :: String -> Integer -> String
fgrow [] n = []
fgrow (x:xs) n = fappend(fcopy n ((x:xs) !! 0)) (fgrow xs (n+1))

-- grow function is used to grow string into required format
-- calling fgrow with string input and start copy it once.
grow :: String -> String
grow [] = []
grow x = fgrow x 1

-- 07
-- tests if the list is in ascending order
-- if the list is empty or has 1 element, return True
--otherwise take the first 2 elements, compare
-- if they are the right order, recursively call instrictorder, else return False
instrictorder :: [Integer]-> Bool
instrictorder [] = True
instrictorder [x] = True
instrictorder (x:(y:ys)) = if (x <= y) then instrictorder (y:ys) else False

-- 08
--  given a list of items and their cost (here just an integer) returns a list of items whose
-- cost is (strictly) below a given threshold costs
-- if the list is empty, return an empty list
-- the function takes a list of tuples, an integer and return a list of string
-- check every second elements of the tuples with the number given
-- if the elementi bigger or equal, recursively call cheapItems
-- otherwise append the first part of element to the list and recursively call function
cheapItems:: [(String,Integer)] -> Integer -> [String]
cheapItems [] n = []
cheapItems (x:xs) n = if (snd x >= n) then (cheapItems xs n) else (fst x):(cheapItems xs n)



--10
--  calculates the list (in ascending order) of all divisors of a positive integer
-- returning the empty list if the number is less than or equal to zero
-- otherwise return a list of integer from 1 to n that x is divisible by n
divisors :: Integer -> [Integer]
divisors n
      | n <= 0 = []
      | otherwise = [x | x <- [1..n], n `rem` x == 0]

-- 11
-- determines whether a given string is a substring of another.

-- Call checkPre function to check if 2 strings has the same prefix
-- recursively call substring to check a given string with the rest of the list
substring :: String -> String -> Bool
substring [] [] = True
substring [] ys = True
substring xs [] = False
substring xs ys
    | checkPre xs ys = True
    | substring xs (tail ys) = True
    | otherwise = False

-- checkPre takes 2 strings as inputs and return Bool
-- If the second string is empty, return false
-- If both strings are empty, return True
-- If both strings are not empty, check the first character.
-- recursively call checkPre for the rest of the strings
checkPre :: String -> String -> Bool
checkPre [] [] = True
checkPre [] ys = True
checkPre xs [] = False
checkPre (x:xs) (y:ys) = (x == y) && checkPre xs ys


-- 12
-- given a list of any type returns the list of all sublists of that list.
-- mapping elements to create powerset and recursively call sublists
sublists :: [a] -> [[a]]
sublists [] = [[]]
sublists (x:xs) = mymap (x:) (sublists xs) ++ sublists xs

-- return a list that every elements applied the function
mymap :: (a->b) -> ([a]->[b])
mymap f [] = []
mymap f(x:xs) = (f x) : (mymap f xs)
