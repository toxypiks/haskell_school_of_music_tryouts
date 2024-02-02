import Euterpea

-- Define a function twice, given a function f, returns a function that applies f twice to its argument.

twice    :: Num a => (a -> a) -> a -> a
twice f y = (f (f y))

my_res_sec = (twice (+1)) 2
