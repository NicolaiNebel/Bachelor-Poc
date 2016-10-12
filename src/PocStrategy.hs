module PocStrategy
  (
    Strategy(..)
  , Op(..)
  , chooseStrategy
  , makeTree
  )
where

import Prelude hiding (traverse)
import PocParser

type Strategy = [Op]

data Op = Swap String String
        | Transpose [String] [Integer]
  deriving (Show, Eq)

-- Currently we assume simple indices, which implies every individual index is invariant to
-- only one variable. This means we can represent an access pattern as just a list of strings.
data AccessTree = Map [String] [AccessTree]
                | Access String [String]
  deriving (Show, Eq)


chooseStrategy :: Program -> Strategy
chooseStrategy = undefined

makeTree :: Program -> [AccessTree]
makeTree = map collapse . traverse

traverse :: Program -> [AccessTree]
traverse = map f
  where f (Loop var stmts) = Map [var] (traverse stmts)
        f (ArrAccess var vars) = Access var vars           -- Redundant, for now. Will expand.

-- Collapse chains of loops 
collapse :: AccessTree -> AccessTree
collapse (Map [v] [t1]) = case (collapse t1) of
                            Map vs t2  -> Map (v:vs) t2
                            t          -> Map [v] [t]
collapse (Map vs ts)  = Map vs (map collapse ts)
collapse t            = t
