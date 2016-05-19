module Task1(interpret, test) where

import Ast

interpret :: String -> Maybe Int
interpret cs = fmap interpret' $ ast cs

interpret' :: Ast -> Int
interpret' (Am n)     = n
interpret' (Op o ods) = foldl f (head vs) (tail vs)
                        where
                            vs = map interpret' ods
                            f  = case o of
                                     Pl -> \x y -> x + y
                                     Ms -> \x y -> x - y
                                     Ml -> \x y -> x * y

test :: [Maybe Int]
test = [interpret s | s <- samples1]
