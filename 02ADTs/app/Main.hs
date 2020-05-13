module Main where

import Lib
import Log

main :: IO ()
main = do
	content <- readFile "error.log"
	mapM (putStrLn . getTxt) (inOrder $ build $ parse content)
