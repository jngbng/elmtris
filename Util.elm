module Util where

-- Creates an infinite list of the specified element
repeat : a -> List a
repeat a = a::(repeat a)

replicate : Int -> a -> List a
replicate n a =
  if n <= 0 then [] else a::(replicate (n-1) a)

zip : List a -> List b -> List (a,b)
zip = List.map2 (\a -> \b -> (a,b))
