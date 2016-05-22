module Task3(interpret_cpu, test) where

import Control.Concurrent
import Control.Monad
import Data.Time.Clock

import Ast
import Job
import Schedule

interpret_cpu :: (String, Int) -> IO (Maybe (Int, Int, Double))
interpret_cpu (s, n) = case ast s of
                           Nothing -> return Nothing
                           Just a  -> do
                                      b      <- getCurrentTime
                                      (r, d) <- interpret_cpu' a n
                                      e      <- getCurrentTime
                                      return $ Just (r, d, realToFrac (diffUTCTime e b) :: Double)

interpret_cpu' :: Ast -> Int -> IO (Int, Int)
interpret_cpu' a n = do
                     printJobs js1
                     printSchedule sl
                     return (1, getTime js1 1)
                     where
                         (js1, sl) = alg a n



test :: IO [Maybe (Int, Int, Double)]
test = mapM interpret_cpu samples2
