module Language.ModPCF.Environment where

import Data.Map (Map)
import qualified Data.Map as M


--
-- * Generic environments
--

-- | An environment maps variables to to some kind of value.
type Env var a = Map var a

-- | The empty environment.
envEmpty :: Env var a
envEmpty = M.empty

-- | Add a binding to the environment.
envAdd :: Ord var => var -> a -> Env var a -> Env var a
envAdd = M.insert

-- | Lookup a binding in the environment.
envGet :: Ord var => var -> Env var a -> Maybe a
envGet = M.lookup

-- | Check whether a binding is in the environment.
envHas :: Ord var => var -> Env var a -> Bool
envHas = M.member
