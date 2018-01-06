module Main where

import MarkovChains.FirstChain
import MarkovChains.Mchain

import System.Environment

import System.IO as I
import System.Directory as D
import Data.Map as M
import Control.Monad
import Control.Monad.State



{-Don't judge the filepath-}
memoryFileName :: FilePath
memoryFileName = "/Users/jonval/WARNING/singularity/PATH/.memories/chain.mem"

groupSz :: Int 
groupSz = 9

makeSureMemoryExistMchain :: IO ()
makeSureMemoryExistMchain = 
  do
     fe <- D.doesFileExist memoryFileName


     unless fe $ do
                print "Creating File"
                handle <- I.openFile memoryFileName I.WriteMode

                I.hPrint handle (M.singleton "Awesome" ["Bot"] :: M.Map String [String])
                I.hClose handle

                  

defaultStart :: M.Map String [String] -> (M.Map String [String], Maybe String)
defaultStart m = (m, Just "")

chainAction :: Mchain String ->  IO ()
chainAction ch =
  do
    handle <- I.openFile memoryFileName I.ReadMode
    state <- I.hGetContents handle
    (answer, (state', _)) <- runStateT ch $ defaultStart (read state)
    putStrLn answer

    {-For some Reason the previous handle is closed by running the previous IO action-}
    {-I don't know why, would love to know why..-}

    handle' <- I.openFile memoryFileName I.WriteMode
    I.hPrint handle' state'
    I.hFlush handle'
    I.hClose handle'


chat :: IO ()
chat = forever $ chainAction $ demoTalkLineWise groupSz



main :: IO ()
main = do 
          makeSureMemoryExistMchain

          args <- getArgs
          case args of 
            [] -> putStr "Learn or Chat & Learn with\n-c to chat linewise\n-l to learn bulk"
            ("-c":_) -> chat
            ("-l":_) -> chainAction $ bulkLearnGroups groupSz
            ("-g":i:_) -> chainAction $ demoGetRandomState >> demoGenerateNGroups (read i)


