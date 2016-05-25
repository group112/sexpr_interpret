module Job(delay, Job(..), Jobs, job, setTime, getTime, setRes, makeJobs, weight, availableJobs, canExec, printJobs) where

import qualified Data.Map as M
import Data.Function
import Data.List
import Data.Maybe
import Text.Printf

import Ast


delay :: Token -> Int
delay Pl = 2
delay Ms = 3
delay Ml = 10


data Job = Job { jobNum  :: Int
               , jobOp   :: Token
               , jobWht  :: Maybe Int
               , jobTime :: Maybe Int
               , jobRes  :: Maybe Int
               , jobPnt  :: Maybe Int
               , jobChn  :: [Int]
               }
               deriving (Eq, Show)

type Jobs = M.Map Int Job


job :: Jobs -> Int -> Job
job js n = fromJust $ M.lookup n js

getWht :: Jobs -> Int -> Int
getWht js n = (\(Just x) -> x) $ jobWht $ job js n

setTime :: Jobs -> Int -> Int -> Jobs
setTime js n t =  M.insert n (job js n){jobTime = Just t} js

getTime :: Jobs -> Int -> Int
getTime js n = (\(Just x) -> x) $ jobTime $ job js n

setRes :: Jobs -> Int -> Int -> Jobs
setRes js n r = M.insert n (job js n){jobRes = Just r} js


makeJobs :: Ast -> Jobs
makeJobs a = M.fromList $ map (\j -> (jobNum j, j)) $ jobs a

jobs :: Ast -> [Job]
jobs a = js
         where
             (js, _, _) = jobs' a [] 1 Nothing

jobs' :: Ast -> [Job] -> Int -> Maybe Int -> ([Job], Int, Int)
jobs' (Op o as) js1 n1 p = ([j] ++ js2, n1, n2)
                           where
                               (cn, js2, n2) = jobs'' as [] js1 (Just n1) (n1 + 1)
                               j             = Job n1 o Nothing Nothing Nothing p cn
jobs' (Am i)    js1 n1 p = ([j] ++ js1, n1, n1 + 1)
                           where
                               j             = Job n1 (Nm 0) Nothing (Just 0) (Just i) p []

jobs'' :: [Ast] -> [Int] -> [Job] -> Maybe Int -> Int -> ([Int], [Job], Int)
jobs'' []     cn js1 _ n1 = (reverse cn, js1, n1)
jobs'' (a:as) cn js1 p n1 = jobs'' as ([c] ++ cn) js2 p n2
                            where
                                (js2, c, n2) = jobs' a js1 n1 p


weight :: Jobs -> Jobs
weight js = weight' js 1

weight' :: Jobs -> Int -> Jobs
weight' js1 n = case jobTime j of
                    Nothing -> M.insert n j{jobWht = Just w2} js2
                    _       -> M.insert n j{jobWht = Just 0 } js1
                where
                    j       = job js1 n
                    js2     = foldl weight' js1 (jobChn j)
                    w1      = (maximum $ map (getWht js2) (jobChn j))
                    w2      = case (jobWht j, w1) of
                                  (Nothing, _) -> (delay $ jobOp j) + w1
                                  (_,       0) -> (delay $ jobOp j)
                                  (Just w,  _) -> w


availableJobs :: Jobs -> Int -> [(Int, Int)]
availableJobs js ct = map (\j -> (j, delay $ jobOp $ job js j)) (availableJobs' js ct 1)

availableJobs' :: Jobs -> Int -> Int -> [Int]
availableJobs' js ct n = case jobTime j of
                             Nothing -> case r of
                                            True -> [jobNum j]
                                            _    -> foldr (\c a -> (availableJobs' js ct c) ++ a) [] chn
                             _       -> []
                         where
                             j   = job js n
                             p v = case v of
                                       Just t | t <= ct -> True
                                       _                -> False
                             r   = not $ elem False $ map p $ map (jobTime . job js) (jobChn j)

                             chn = reverse $ sortBy (compare `on` getWht js) (jobChn j)


canExec :: Jobs -> Int -> (Bool, Token, [Int])
canExec js n = (a, jobOp j, rs)
               where
                   j   = job js n
                   rs' = map (\c -> jobRes $ job js c) (jobChn j)
                   a   = not $ elem Nothing rs'
                   rs  = case a of
                             True -> map fromJust rs'
                             _    -> []


printJobs :: Jobs -> IO ()
printJobs js1 = do
                printf "---------------------------------------------------------------------------------\n"
                printf "Job |  Op   |  Weight   |   Time    |  Result   |  Parent   |     Children       \n"
                printf "---------------------------------------------------------------------------------\n"
                printJobs' js2
                where
                    (_, js2) = unzip $ M.toList js1

printJobs' :: [Job] -> IO ()
printJobs' []     = return ()
printJobs' (j:js) = do
                    printf "%3d | %5s | %9s | %9s | %9s | %9s | %s\n" (jobNum j) o (s $ jobWht j) (s $ jobTime j) (s $ jobRes j) (s $ jobPnt j) (show $ jobChn j)
                    printJobs' js
                    where
                        o = case jobOp j of
                                Pl   -> "+"
                                Ms   -> "-"
                                Ml   -> "*"
                                Nm _ -> "int"

                        s m = case m of
                                Just x -> show x
                                _      -> "-"
