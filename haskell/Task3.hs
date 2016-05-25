module Task3(interpret_cpu, test) where

import Control.Concurrent.Chan
import Control.Concurrent
import Control.Monad
import Data.Time.Clock
import Data.Maybe
import qualified Data.Map as M

import Ast
import Job
import Schedule


data Req = Req { reqJob :: Int
               , reqOp  :: Token
               , reqOds :: [Int]
               }

data Res = Res { resJob :: Int
               , resVal :: Int
               }


workerCPU :: Chan Req -> Chan Res -> IO ()
workerCPU crq crs = do
                    r <- readChan crq
                    threadDelay $ 10^6 * (delay $ reqOp r)
                    writeChan crs $ Res (reqJob r) (v r)
                    workerCPU crq crs
                    where
                        v r' = foldl (f r') (head $ reqOds r') (tail $ reqOds r')
                        f r' = case (reqOp r') of
                                   Pl -> \x y -> x + y
                                   Ms -> \x y -> x - y
                                   Ml -> \x y -> x * y

createCPUs :: Int -> IO ([Chan Req], Chan Res)
createCPUs n = do
               crs  <- newChan
               crqs <- mapM (\_ -> newChan) [1..n]
               mapM_ (\crq -> forkIO $ workerCPU crq crs) crqs
               return (crqs, crs)


interpret_cpu :: (String, Int) -> IO (Maybe (Int, Int, Double))
interpret_cpu (s, n) = case ast s of
                           Nothing -> return Nothing
                           Just a  -> do
                                      printJobs js
                                      printSchedule sl
                                      b <- getCurrentTime
                                      r <- interpret_cpu' js sl
                                      e <- getCurrentTime
                                      return $ Just (r, getTime js 1, realToFrac (diffUTCTime e b) :: Double)
                                      where
                                          (js, sl) = alg a n

interpret_cpu' :: Jobs -> ShortSchedule -> IO Int
interpret_cpu' js sl = do
                       (crqs, crs) <- createCPUs $ M.size sl
                       calculate js sl crqs crs


calculate :: Jobs -> ShortSchedule -> [Chan Req] -> Chan Res -> IO Int
calculate js sl crqs crs = case jobRes $ job js 1 of
                               Just v -> return v
                               _      -> do
                                         mapM_ (\(cn, rq) -> writeChan (crqs !! (cn - 1)) rq) rqs
                                         rs <- readChan crs
                                         calculate (setRes js (resJob rs) (resVal rs)) sl' crqs crs
                                         where
                                             (rqs, sl') = jobForCPU js sl

jobForCPU :: Jobs -> ShortSchedule -> ([(Int, Req)], ShortSchedule)
jobForCPU js sl = M.foldlWithKey f ([], sl) sl
                  where
                      f (rqs, sl') cn _ = (rq ++ rqs, sl'')
                                          where
                                              (rq, sl'') = jobForCPU' js sl' cn

jobForCPU' :: Jobs -> ShortSchedule -> Int -> ([(Int, Req)], ShortSchedule)
jobForCPU' js sl cn = case jns of
                          (jn:jns') -> case canExec js jn of
                                           (True, op, ods) -> ([(cn, Req jn op ods)], M.insert cn jns' sl)
                                           _               -> ([], sl)
                          _         -> ([], sl)
                      where
                          jns = fromJust $ M.lookup cn sl


test :: IO [Maybe (Int, Int, Double)]
test = mapM interpret_cpu samples2
