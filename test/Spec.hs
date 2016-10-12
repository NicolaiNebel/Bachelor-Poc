import System.Directory

import PocParser

main :: IO ()
main = do
  files <- getDirectoryContents "test/basic_programs"
  mapM_ (parseTest "test/basic_programs/") files

parseTest :: FilePath -> FilePath -> IO ()
parseTest _ "." = return ()
parseTest _ ".." = return ()
parseTest p f = do
  program <- readFile (p++f)
  print $ parseProg f program
