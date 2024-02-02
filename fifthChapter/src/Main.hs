-- Exercise 5.1 Define a function twice, given a function f, returns a function that applies f twice to its argument.

twice    :: Num a => (a -> a) -> a -> a
twice f y = (f (f y))

my_res = (twice (+1)) 2

-- Exercise 5.2 Define a function power that takes a function f and an integer n and returns a function that applies the function f to its argument n times.

power :: (Num a, Eq a) => (a -> a) -> a -> a -> a
power f y 0 = y
power f y n = let x = (f y)
              in power f x (n-1)

my_res_sec = power (+2) 5 1
