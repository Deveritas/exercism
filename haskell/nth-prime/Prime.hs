module Prime where

nth :: Int -> Int
nth = run 2
    where run c a = case drop (a-1) (atkin c) of
                        (x:_) -> x
                        _     -> run (c*2) a

atkin :: Int -> [Int]
atkin n = sieve [2..n]
    where sieve [] = []
          sieve (p:is) = p : sieve (is `minus` [p*p, p*p+p..n])
          minus (l:ls) (r:rs)
            | l < r = l : (ls `minus` (r:rs))
            | l == r = ls `minus` rs
            | l > r = (l:ls) `minus` rs
          minus xs _ = xs