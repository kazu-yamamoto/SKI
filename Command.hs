module Command where

import Data.IORef
import Env
import Eval
import Parser
import System.IO.Unsafe

----------------------------------------------------------------

envRef :: IORef Env
envRef = unsafePerformIO $ newIORef initEnv

set :: Env -> IO ()
set = writeIORef envRef

get :: IO Env
get = readIORef envRef

----------------------------------------------------------------

eval :: String -> IO ()
eval cs = eval' cs >>= putStrLn

eval' :: String -> IO String
eval' cs = do
    env <- get
    let exp0 = parseExp cs env
        exp1 = evaluate exp0
    return $ show exp1

----------------------------------------------------------------

def :: String -> IO ()
def cs = do
    env <- get
    let (c,exp0) = parseDef cs env
        env' = define c exp0 env
    set env'

clear :: IO ()
clear = set initEnv
