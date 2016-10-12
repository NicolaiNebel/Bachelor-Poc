import System.Directory

import PocParser
import PocStrategy

main :: IO ()
main = do
  files <- getDirectoryContents "test/basic_programs"
  mapM_ (progTest "test/basic_programs/") files

progTest :: FilePath -> FilePath -> IO ()
progTest _ "." = return ()
progTest _ ".." = return ()
progTest p f = do
  program <- readFile (p++f)
  let p = parseProg f program
  print p
  case p of
    Right p -> print $ makeTree p
    Left e -> return ()
