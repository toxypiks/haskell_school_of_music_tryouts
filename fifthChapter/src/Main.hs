-- 5.1 Sections
-- Exercise 5.1 Define a function twice, given a function f, returns a function that applies f twice to its argument.

-- First solution

twice    :: Num a => (a -> a) -> a -> a
twice f y = (f (f y))

my_res = (twice (+1)) 2

-- Actually returning a function using where

twice_sec :: Num a => (a -> a) -> (a -> a)
twice_sec f = twiceF
          where twiceF a = f (f a)

my_res_sec = (twice_sec (+1)) 2


-- Exercise 5.2 Define a function power that takes a function f and an integer n and returns a function that applies the function f to its argument n times.

power :: (Num a, Eq a) => (a -> a) -> a -> a -> a
power f y 0 = y
power f y n = let x = (f y)
              in power f x (n-1)

my_res_pow = power (+2) 5 1

-- Actually returning a function using where together with recursion

power_secc :: (Num a, Eq a) => (a -> a) -> a -> (a -> a)
power_secc f n = powerF n
               where powerF n y | (n == 1) = f y
                                | otherwise = powerF (n-1) (f y)

my_res_pow_fct = power_secc (+2) 5

my_res_pow_ergebnis = my_res_pow_fct 1
