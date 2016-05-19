module Ast(Token(..), Ast(..), ast, samples1, samples2) where

data Token = Pl
           | Ms
           | Ml
           | Nm Int
           | Ed
           | Er
           deriving (Eq, Show)

data Ast = Op Token [Ast]
         | Am Int 
         deriving (Eq, Show)

tokens :: String -> [Token]
tokens cs = reverse $ tokens' cs [] 

tokens' :: String -> [Token] -> [Token]
tokens' []         ts = ts
tokens' (' ':cs)   ts = tokens' cs ts
tokens' (')':cs)   ts = tokens' cs $ [Ed] ++ ts
tokens' ('(':c:cs) ts = tokens' cs $ [t]  ++ ts 
                        where
                            t = case c of
                                   '+' -> Pl
                                   '-' -> Ms
                                   '*' -> Ml
                                   _   -> Er 
tokens' cs1        ts = tokens' cs2 $ [t] ++ ts
                        where
                            (t, cs2) = case reads cs1 :: [(Int,String)] of
                                           [(n, cs3)] -> (Nm n, cs3)
                                           _          -> (Er,   [] )

ast :: String -> Maybe Ast
ast cs = case ast' $ tokens cs of
             Just (a, []) -> Just a
             _            -> Nothing 

ast' :: [Token] -> Maybe (Ast, [Token])
ast' (Nm n:ts) = Just (Am n, ts)
ast' (Pl  :ts) = ast'' Pl [] ts
ast' (Ms  :ts) = ast'' Ms [] ts
ast' (Ml  :ts) = ast'' Ml [] ts
ast' _         = Nothing

ast'' :: Token -> [Ast] -> [Token] -> Maybe (Ast, [Token])
ast'' _ _   []      = Nothing
ast'' o ods (Ed:ts) = Just (Op o (reverse ods), ts) 
ast'' o ods ts1     = case ast' ts1 of
                         Just (od, ts2) -> ast'' o ([od] ++ ods) ts2
                         _              -> Nothing

sample_1 :: String
sample_1 = "(+ (* 4 4) (* 2 (- 7 5)) 1)"

sample_2 :: String
sample_2 = "10"

sample_3 :: String
sample_3 = "(* 10 (- 0 1))"

sample_4 :: String
sample_4 = "(- (+ 10 10) -5 0)"

sample_5 :: String
sample_5 = "(+ (- (* (+ (- (* 1))))))"

sample_6 :: String
sample_6 = "(* 2 (+ (- 10 9) (- 3 (* 2 1))) (+ (- 10 9) (- 3 (* 2 1))))"

sample_7 :: String
sample_7 = "(+ (* 2 1) (+ 8 8) (- (+ 4 3 2 1) (* 3 3) (* 2 2)) (* 5 7))"

sample_8 :: String
sample_8 = "(- (+ (+ 3 3) (- 3 3) (+ 3 3) (- 3 3)) (* 2 2))"

sample_9 :: String
sample_9 = "(+ (- 6 1) (+ 0 1 1) (- 7 2) (* 3 4 5) (- 3 1) (+ 2) (- 0 10))"

samples1 :: [String]
samples1 = [ sample_1
           , sample_2
           , sample_3
           , sample_4
           , sample_5
           , sample_6
           , sample_7
           , sample_8
           , sample_9] 

samples2 :: [(String, Int)]
samples2 =  [ (sample_1, 2)
            , (sample_2, 2)
            , (sample_3, 2)
            , (sample_4, 2)
            , (sample_5, 2)
            , (sample_6, 2)
            , (sample_6, 3)
            , (sample_7, 2)
            , (sample_7, 3)
            , (sample_7, 4)
            , (sample_8, 2)
            , (sample_8, 3)
            , (sample_9, 2)]

