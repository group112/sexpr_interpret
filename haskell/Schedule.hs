module Schedule(delay) where

import Ast

delay :: Token -> Int
delay Pl = 2
delay Ms = 3
delay Ml = 10

