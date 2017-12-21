module Main where

import MarkovChains

import System.IO as I
import System.Directory as D
import Data.Map as M
import Control.Monad



haveAchat :: IO ()
haveAchat = do 
               file <- readFile memoryname 
               transit converse (read file)
               return ()



makeSureMemoryExist :: IO ()
makeSureMemoryExist = do
                       fe <- D.doesFileExist memoryname


                       unless fe $ do
                                  print "Creating File"
                                  handle <- I.openFile memoryname I.WriteMode

                                  I.hPrint handle (([], M.empty) :: ([String], M.Map String [String]))
                                  I.hClose handle

                  



main :: IO ()
main = do 

          makeSureMemoryExist


          haveAchat

