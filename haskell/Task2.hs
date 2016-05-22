module Task2(interpret_network, test) where

import Ast
import Job

interpret_network :: String -> Maybe (Int, Int)
interpret_network cs = fmap interpret_network' $ ast cs

interpret_network' :: Ast -> (Int, Int)
interpret_network' (Am n)     = (n, 0)
interpret_network' (Op o ods) = (v, t)
                                where
                                    v        = foldl f (head vs) (tail vs)
                                    t        = delay o  + maximum ts
                                    (vs, ts) = unzip $ map interpret_network' ods   
                                    f        = case o of
                                                   Pl -> \x y -> x + y
                                                   Ms -> \x y -> x - y
                                                   Ml -> \x y -> x * y

test :: [Maybe (Int, Int)]
test = [interpret_network s | s <- samples1]
