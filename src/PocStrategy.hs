module PocStrategy where

import PocParser

import Control.Monad
import Data.Monoid
import qualified Data.Map.Lazy as Map
import Data.List (nub, sort)
import Data.Maybe

type Var = String

type In a = Maybe a
type Out a = [a]
type Tr = (In Int, Out Int)
type ArrMap = Map.Map Var Tr


data Access = Access { interIn :: In Var
                     , interOut :: Out Var
                     , transposes :: ArrMap
                     }
  deriving (Show, Eq)

unit :: Access
unit = Access Nothing [] Map.empty

plus :: Access -> Access -> Maybe Access
a1 `plus` a2 = do
  interIn <- joinIn (interIn a1) (interIn a2)

  interOut <- joinOut (interOut a1) (interOut a2) 
  when (isJust interIn && (fromJust interIn) `elem` interOut) Nothing

  tr <- joinTr (transposes a1) (transposes a2)
  Just $ Access interIn interOut tr

joinIn :: (Eq a) => In a -> In a -> Maybe (In a)
joinIn x Nothing = return x
joinIn Nothing y = return y
joinIn (Just a) (Just b) = if a == b then return (Just a) else Nothing

joinOut :: (Ord a) => Out a -> Out a -> Maybe (Out a)
joinOut xs ys = return zs
  where zs = sort . nub $ xs ++ ys

joinTr :: ArrMap -> ArrMap -> Maybe ArrMap
joinTr m1 m2 = foldM help m1 pairs
  where pairs = Map.toList m2

help :: ArrMap -> (Var, Tr) -> Maybe ArrMap
help m (v,t1) = case Map.lookup v m of
                Nothing -> return $ Map.insert v t1 m
                Just t2 -> do t <- joinTransposes t1 t2
                              return $ Map.insert v t m

joinTransposes :: Tr -> Tr -> Maybe Tr
joinTransposes (i1, o1) (i2, o2) = do
  i1' <- i1
  i2' <- i2
  if i1' /= i2' 
    then Nothing
    else do
      let o3 = sort . nub $ o1 ++ o2
      return (i1, o3)

chooseStrategy :: Program -> [Access]
chooseStrategy = undefined 

-- [LoopVars] ArrAccess -> [Initial As]
generate :: [Var] -> Stmt -> [Access]
generate lvs (ArrAccess arr is) = catMaybes (out : ins)
  where ins = map (pushIn arr is) variants                 -- Coalesced access
        out = Just $ Access Nothing variants Map.empty -- Invariant access
        variants = filter (isVariantTo is) lvs

pushIn :: String -> [Index] -> String -> Maybe Access
pushIn arr is lv = case variantIndices is lv of
                    []  -> Just $ Access (Just lv) [] Map.empty
                            -- Access is invariant to lv
                    [x] -> Just $ Access (Just lv) [] (Map.singleton arr (Just x, []))
                            -- Access is variant in one index.
                    _   -> Nothing
                            -- Access is variant in more indexes to lv. No use in pushing in.

pushOut :: [Index] -> [String] -> Maybe Access
pushOut is lvs = Just $ Access Nothing (sort . filter (isVariantTo is) $ lvs) Map.empty

isVariantTo :: [Index] -> String -> Bool
isVariantTo is var = variantIndices is var /= []

-- These indices are numbers! Positions in the array!
-- From innermost to outermost! That is, 1 is the position of the relevant access.
variantIndices :: [Index] -> String -> [Int]
variantIndices is var = filter (\x -> (is !! x) == Var var) [0..length is-1]
