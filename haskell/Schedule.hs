module Schedule(printSchedule, alg) where

import qualified Data.Map.Strict as M
import Data.List
import Text.Printf

import Ast
import Job
import Packing


data Unit = Jb Int Int
          | Il Int 

isJob :: Unit -> Bool
isJob (Jb _ _) = True
isJob _        = False

unitTime :: Unit -> Int
unitTime (Jb _ t) = t
unitTime (Il t)   = t 


initSchedule :: Int -> M.Map Int [Unit]
initSchedule n = foldl (\a k -> M.insert k [Il 0] a) M.empty [1..n]

currentTime :: M.Map Int [Unit] -> Int
currentTime sl = minimum $ M.foldl (\a v -> [unitTime $ head v] ++ a) [] sl

nextTime :: M.Map Int [Unit] -> Int
nextTime sl = minimum $ filter (\v -> v /= currentTime sl) $ M.foldl (\a v -> [unitTime $ head v] ++ a) [] sl

squeezeSchedule :: M.Map Int [Unit] -> M.Map Int [Int]
squeezeSchedule sl = M.map f sl
                     where
                         f us = reverse $ map (\(Jb n _) -> n) $ filter isJob us

availableCPUs :: M.Map Int [Unit] -> [Int]
availableCPUs sl = M.foldlWithKey f [] sl
                   where
                       f a k v = if (unitTime $ head v) == currentTime sl then [k] ++ a else a

idleCPUs :: M.Map Int [Unit] -> M.Map Int [Unit]
idleCPUs sl = idleCPUs' sl (nextTime sl) (availableCPUs sl)                           

idleCPUs' :: M.Map Int [Unit] -> Int -> [Int] -> M.Map Int [Unit]
idleCPUs' sl1 nt []     = sl1
idleCPUs' sl1 nt (c:cs) = idleCPUs' sl2 nt cs
                          where
                              sl2 = M.insert c ([Il nt] ++ v) sl1
                              v   = (\(Just x) -> x) (M.lookup c sl1)

loadCPUs :: M.Map Int [Unit] -> [(Int, Int)]
loadCPUs sl = M.foldlWithKey f [] sl
              where
                  f a k v = [(k, unitTime $ head v)] ++ a

updateSchedule :: M.Map Int [Unit] -> Int -> Int -> Int -> (M.Map Int [Unit], Int)
updateSchedule sl c j t1 = (M.insert c ([Jb j t2] ++ v) sl, t2)
                           where
                               t2 = t1 + (unitTime $ head v)
                               v  = (\(Just x) -> x) (M.lookup c sl)

printSchedule :: M.Map Int [Int] -> IO ()
printSchedule sl = do
                   printf "---------------------------------------------------------------------------------\n"
                   printf "CPU |        Schedule                                                            \n"
                   printf "---------------------------------------------------------------------------------\n"
                   printSchedule' $ M.toList sl

printSchedule' :: [(Int, [Int])] -> IO ()
printSchedule' [] = return ()
printSchedule' ((c, js):sl) = do
                              printf "%3d | %s\n" c (foldl (\a j -> a ++ "    " ++ show j) "" js)
                              printSchedule' sl


select1 :: M.Map Int Job -> M.Map Int [Unit] -> ([((Int, Int), Int)], [Int])
select1 js sl = (zip (take l ajs) wcs, ics)
                where
                    ajs        = availableJobs js $ currentTime sl
                    acs        = availableCPUs sl
                    l          = min (length ajs) (length acs)
                    (wcs, ics) = splitAt l acs
                   

select2 :: M.Map Int Job -> M.Map Int [Unit] -> ([((Int, Int), Int)], [Int])
select2 js sl = (jcs, ics)
                where
                    ajs      = availableJobs js $ currentTime sl
                    acs      = availableCPUs sl
                    jcs      = filter (\((_, _), c) -> elem c acs) $ packing ajs (loadCPUs sl)
                    (_, wcs) = unzip jcs
                    ics      = acs \\ wcs


performJobs :: M.Map Int Job -> M.Map Int [Unit] -> [((Int, Int), Int)] -> [Int] ->  (M.Map Int Job, M.Map Int [Unit])
performJobs js1 sl1 wcs ics = (js2, sl3)
                              where
                                  (js2, sl2) = foldl f (js1, sl1) wcs
                                  sl3 = if length ics > 0 then idleCPUs sl2 else sl2

                                  f (js, sl) ((j, t), c) = (js', sl')
                                                           where
                                                               (sl', t') = updateSchedule sl c j t
                                                               js'       = setTime js j t'  

alg :: Ast -> Int -> (M.Map Int Job, M.Map Int [Int])
alg a n = (js, squeezeSchedule sl)
          where
              (js, sl) = alg' (weight $ makeJobs a) (initSchedule n)

alg' :: M.Map Int Job -> M.Map Int [Unit] -> (M.Map Int Job, M.Map Int [Unit])
alg' js sl = case jobTime $ job js 1 of
                   Just _ -> (js, sl)
                   _      -> case wcs of
                                 [] -> alg' js $ idleCPUs sl
                                 _  -> alg' (weight js2) sl2
             where
                 (wcs, ics) = select2 js sl
                 (js2, sl2) = performJobs js sl wcs ics 
