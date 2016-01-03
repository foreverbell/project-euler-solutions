#!/usr/bin/env runghc

import Data.List (sort, intercalate)
import System.Directory (getDirectoryContents)
import System.Environment (getArgs)
import Text.Printf (printf)

template = unlines [ "executable %d"
                   , "  hs-source-dirs:      Src"
                   , "  main-is:             %d.hs"
                   , "  ghc-options:         -threaded -O2 -fllvm -W -fwarn-tabs"
                   , "  build-depends:       %s"
                   , "  default-language:    Haskell2010"
                   ] ++ "\n"

packages = intercalate ", " [ "base"
                            , "project-euler-solutions"
                            , "array", "vector", "containers"
                            , "text"
                            , "time", "parallel", "deepseq"
                            , "data-memocombinators"
                            ]

dropSuffix :: FilePath -> FilePath
dropSuffix f = take (length f - 3) f

filterDots :: [FilePath] -> [FilePath]
filterDots fs = filter (\p -> p /= "." && p /= "..") fs

main = do
  args <- getArgs
  fs <- case args of
             [] -> map (read . dropSuffix) <$> filterDots <$> getDirectoryContents "Src/" :: IO [Int]
             xs -> return $ map read args
  putStrLn $ printf "found %d hs files." (length fs)
  templateIn <- readFile "project-euler-solutions.cabal.in"
  writeFile "project-euler-solutions.cabal" $ templateIn ++ concatMap (\d -> printf template d d packages) (sort fs)
  putStrLn "project-euler-solutions.cabal is successfully generated."
