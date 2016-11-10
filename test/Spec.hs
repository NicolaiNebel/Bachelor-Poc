import System.Directory

import PocParser
import PocStrategy

import StrategyTest

-- Test GENERATE --
main :: IO ()
main = do
  mapM_ (uncurry generateTest) genTests
  plusTest

-- PARSER AND PROGRAMS STUFF --
--main :: IO ()
--main = do
--  files <- getDirectoryContents "test/basic_programs"
--  mapM_ (progTest "test/basic_programs/") files
--
--progTest :: FilePath -> FilePath -> IO ()
--progTest _ "." = return ()
--progTest _ ".." = return ()
--progTest p f = do
--  program <- readFile (p++f)
--  print ("Parsing " ++ (p++f))
--  let p = parseProg f program
--  print p
