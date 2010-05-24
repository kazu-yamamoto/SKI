module Env (
    Env
  , resolve, define
  , initEnv
  ) where

import qualified Data.Map as M
import Data.Maybe
import Exp

type Env = M.Map Char Exp

resolve :: Char -> Env -> Exp
resolve c env = fromMaybe (error $ c : " is not defined") (M.lookup c env)

define :: Char -> Exp -> M.Map Char Exp -> M.Map Char Exp
define = M.insert

initEnv :: Env
initEnv = M.fromList [
    ('S',S)
  , ('K',K)
  ]
