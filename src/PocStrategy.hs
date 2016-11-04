module PocStrategy where

import PocParser

import Data.Monoid
import qualified Data.Map.Lazy as Map
import Data.List (nub, sort)

type Var = String

type In a = Maybe (Maybe a)
type Out a = [a]
type Tr = (In Int, Out Int)
type ArrMap = Map.Map Var Tr

data Access = Access { interIn :: In Var
                     , interOut :: Out Var
                     , transposes :: ArrMap
                     }

instance Monoid Access where
  mempty = Access (Just Nothing) [] Map.empty
  p1 `mappend` p2 = Access in' out' tr'
    where in' = case (interIn p1, interIn p2) of
                  (Just i1, Just i2) -> Just $ join i1 i2
                  _                  -> Nothing
          out' = sort $ nub (interOut p1 ++ interOut p2)
          tr' = Map.unionWith joinTransposes (transposes p1) (transposes p2)


join :: (Eq a) => Maybe a -> Maybe a -> Maybe a
join x Nothing = x
join Nothing y = y
join (Just a) (Just b) = if a == b then Just a else Nothing

joinTransposes :: Tr -> Tr -> Tr
joinTransposes (i1, o1) (i2, o2) = (i3, o3)
  where i3 = case (i1, i2) of
              (Just x, Just y) -> if x == y then Just x else Nothing
              (Nothing, y) -> y
              (x, Nothing) -> x
        o3 = sort $ nub (o1 ++ o2)

chooseStrategy :: Program -> [Access]
chooseStrategy = undefined 

generate :: [Var] -> Stmt -> [Access]
generate = undefined
