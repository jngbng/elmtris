module Util where

import List ((::))

-- Creates an infinite list of the specified element
repeat : a -> List a
repeat a = a::(repeat a)

replicate : Int -> a -> List a
replicate n a =
  if n <= 0 then [] else a::(replicate (n-1) a)
