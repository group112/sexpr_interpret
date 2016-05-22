module Task2_(interpret_network, test) where

import Control.Concurrent
import Control.Monad
import Data.Time.Clock

import Ast
import Job

interpret_network :: String -> IO (Maybe (Int, Double))
interpret_network s = case ast s of
                          Nothing -> return Nothing 
                          Just a  -> do
                                     b <- getCurrentTime
                                     r <- interpret_network' a
                                     e <- getCurrentTime
                                     return $ Just (r, realToFrac (diffUTCTime e b) :: Double)

interpret_network' :: Ast -> IO Int 
interpret_network' (Am n)     = return $ n
interpret_network' (Op o ods) = do
                                rs <- mapM run ods
                                vs <- mapM takeMVar rs
                                r  <- newEmptyMVar
                                serv o vs r
                                where
                                    run a = do
                                            r <- newEmptyMVar
                                            forkIO $ clnt a r
                                            return r 

clnt :: Ast -> MVar Int -> IO ()
clnt a r = do
           v <- interpret_network' a
           putMVar r v

calc :: Token -> [Int] -> MVar Int -> IO ()
calc o vs r = do
              threadDelay $ 10^6 * delay o 
              putMVar r v
              where
                  v = foldl f (head vs) (tail vs)
                  f = case o of
                          Pl -> \x y -> x + y
                          Ms -> \x y -> x - y
                          Ml -> \x y -> x * y

serv :: Token -> [Int] -> MVar Int -> IO (Int)
serv o vs r = do
              forkIO $ calc o vs r 
              takeMVar r

test :: IO [Maybe (Int, Double)]
test = mapM interpret_network samples1
