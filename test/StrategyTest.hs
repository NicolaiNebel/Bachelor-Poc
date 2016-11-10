module StrategyTest where

import PocParser
import PocStrategy

import Data.Map.Lazy (fromList, empty)

generateTest :: [Var] -> Stmt -> IO ()
generateTest is acc = mapM_ (putStrLn . show) $ generate is acc

genTests = [ (["p1","p2","p3"], ArrAccess "A" [Var "p3", Inv, Var "p1"])
           , (["p1", "p2"], ArrAccess "B" [Inv, Inv])
           ]


plusTest = mapM_ plusShow (zip3 plusTestData plusTestExpect plusTestMsg)

plusShow :: ((Access, Access), Maybe Access, String) -> IO ()
plusShow (args, expect, msg) = do
  if (uncurry plus) args == expect 
    then putStr "Ok: "
    else putStr "Not ok: "
  putStrLn msg

plusTestData = [ ( Access (Just "p1") [] empty
                 , Access (Just "p1") [] empty -- interIn agree
                 )
               , ( Access Nothing [] empty
                 , Access Nothing [] empty -- interIn both Nothing
                 )
               , ( Access (Just "p1") [] empty
                 , Access Nothing [] empty -- One Just
                 )
               , ( Access Nothing [] empty
                 , Access (Just "p2") [] empty -- One Just
                 )
               , ( Access (Just "p1") [] empty
                 , Access (Just "p2") [] empty -- Different Just
                 )
               , ( Access Nothing ["p1"] empty
                 , Access Nothing ["p2"] empty -- outerIn
                 )
               , ( Access Nothing ["p2"] empty
                 , Access Nothing ["p1"] empty -- outerIn
                 )
               , ( Access Nothing ["p1"] empty
                 , Access Nothing ["p1"] empty -- outerIn
                 )
               , ( Access Nothing [] ( fromList [("A",(Just 2,[]))] )
                 , Access Nothing [] ( fromList [("B",(Just 2,[]))] ) -- Combine two diff arrays
                 )
               , ( Access Nothing [] ( fromList [("A",(Just 2,[]))] )
                 , Access Nothing [] ( fromList [("A",(Just 2,[]))] ) -- Combine two eq arrays
                 )
               , ( Access Nothing [] ( fromList [("A",(Just 2,[]))] )
                 , Access Nothing [] ( fromList [("A",(Just 0,[]))] ) -- Fail on combining
                 )
                ]

plusTestExpect = [ Just $ Access (Just "p1") [] empty -- interIn agree
                 , Just $ Access Nothing [] empty
                 , Just $ Access (Just "p1") [] empty
                 , Just $ Access (Just "p2") [] empty
                 , Nothing
                 , Just $ Access Nothing ["p1","p2"] empty -- outerIn
                 , Just $ Access Nothing ["p1","p2"] empty
                 , Just $ Access Nothing ["p1"] empty
                 , Just $ Access Nothing [] ( fromList [("A",(Just 2,[])) -- transposes
                                                       ,("B",(Just 2,[]))
                                                       ] ) -- Combine two diff arrays
                 , Just $ Access Nothing [] ( fromList [("A",(Just 2,[]))] )
                 , Nothing
                 ]

plusTestMsg = [ "interIn: Same Just"
              , "interrIn: Both Just"
              , "interIn: First Just"
              , "interIn: Second Just"
              , "interIn: Different Just"
              , "outerIn: Combine singleton lists"
              , "outerIn: Combine singleton lists unsorted"
              , "outerIn: Combine two eq lists"
              , "transposes: Two diff arrays"
              , "transposes: Combine two eq arrays"
              , "transposes: Fail on combining"
              ]


