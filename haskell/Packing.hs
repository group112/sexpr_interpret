module Packing(packing) where

import qualified Data.Map.Strict as M
import Data.Function
import Data.List


data Bin = Bin { binNum  :: Int
               , binLoad :: Int
               , binElms :: [(Int, Int)]
               }


bin :: M.Map Int Bin -> Int -> Bin
bin bs n = (\(Just x) -> x) $ M.lookup n bs

weight :: [(Int, Int)] -> Int
weight js = foldl (\a (_, t) -> a + t) 0 js

binWeight :: Bin -> Int
binWeight b = (binLoad b) + (weight $ binElms b)


initBins :: [(Int, Int)] -> M.Map Int Bin
initBins cs = M.fromList [(c, Bin c l []) | (c, l) <- cs]

fillBins :: M.Map Int Bin -> [(Int, Int)] -> M.Map Int Bin
fillBins bs []     = bs
fillBins bs (p:js) = fillBins (fillBins' bs p) js
                     where
                         fillBins' bs' p' = M.insert n b{binElms = (binElms b) ++ [p']} bs'
                                            where
                                                (n, _) = head $ sortBy (compare `on` \(x, y) -> y) (M.toList $ M.map binWeight bs')
                                                b      = bin bs' n


split :: M.Map Int Bin -> Int -> Int -> ([(Int, Int)], [(Int, Int)], Int, Int)
split bs c n = (les, res, binLoad b + weight les, weight res)
               where
                   b          = bin bs c
                   (les, res) = splitAt n $ binElms b

swap :: M.Map Int Bin -> Int -> Int -> (M.Map Int Bin, Bool)
swap bs c1 c2 = swap' bs c1 c2 0 0

swap' :: M.Map Int Bin -> Int -> Int ->  Int -> Int -> (M.Map Int Bin, Bool)
swap' bs c1 _  n1 _  | n1 > (length $ binElms $ bin bs c1) = (bs, False)
swap' bs c1 c2 n1 n2 | n2 > (length $ binElms $ bin bs c1) = swap' bs c1 c2 (n1 + 1) 1
swap' bs c1 c2 n1 n2                                       = case cw > nw of
                                                                 True -> (bs', True)
                                                                 _    -> swap' bs c1 c2 n1 (n2 + 1) 
                                                             where
                                                                 (les1, res1, lwt1, rwt1) = split bs c1 n1
                                                                 (les2, res2, lwt2, rwt2) = split bs c2 n2
                                                                 cw                       = max (lwt1 + rwt1) (lwt2 + rwt2) 
                                                                 nw                       = max (lwt1 + rwt2) (lwt2 + rwt1)
                                                                 b1 = (bin bs c1){binElms = les1 ++ res2} 
                                                                 b2 = (bin bs c2){binElms = les2 ++ res1} 
                                                                 bs' = M.insert c2 b2 $ M.insert c1 b1 bs

minimize :: M.Map Int Bin -> M.Map Int Bin
minimize bs = minimize' bs 1 2

minimize' :: M.Map Int Bin -> Int -> Int -> M.Map Int Bin
minimize' bs c1 _  | c1 > M.size bs = bs
minimize' bs c1 c2 | c2 > M.size bs = minimize' bs (c1 + 1) (c1 + 2)
minimize' bs c1 c2                  = case swap bs c1 c2 of
                                          (bs', True) -> minimize bs'
                                          _           -> minimize' bs c1 (c2 + 1)


packing :: [(Int, Int)] -> [(Int, Int)] -> [((Int, Int), Int)] -- [(j, t)] -> [(c, l)] -> [((j, t), c)]
packing js cs = bs3
                where
                    bs1 = minimize $ fillBins (initBins cs) js 
                    bs2 = filter (\(_, b) -> (length $ binElms b) > 0) $ M.toList bs1
                    bs3 = map (\(c, b) -> (head $ binElms b, c)) bs2


