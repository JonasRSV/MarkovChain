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

defaultMemory :: IO ()
defaultMemory = do
                print "Creating File"
                handle <- I.openFile memoryFileName I.WriteMode

                I.hPrint handle (M.singleton "Awesome" ["Bot"] :: M.Map String [String])
                I.hClose handle



makeSureMemoryExistMchain :: IO ()
makeSureMemoryExistMchain = 
  do
     fe <- D.doesFileExist memoryFileName
     unless fe defaultMemory                 


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


chat :: Int -> IO ()
chat groupSz = forever $ chainAction $ demoTalkLineWise groupSz



main :: IO ()
main = do 
          makeSureMemoryExistMchain

          args <- getArgs
          case args of 
            [] -> putStr "Learn, Chat or Generate with\n{-c GROUP-SIZE} to chat linewise with groups of size GROUP-SIZE\n{-l GROUP-SIZE} to learn groups of size GROUP-SIZE \n{-g G} to generate G Groups\n{-d} Restore Memory to Default"
            ("-c":gz:_) -> chat (read gz)
            ("-l":gz:_) -> chainAction $ bulkLearnGroups (read gz)
            ("-g":i:_) -> chainAction $ demoGetRandomState >> demoGenerateNGroups (read i)
            ("-d":_) -> defaultMemory >> putStrLn "Restored to Default Memory"


