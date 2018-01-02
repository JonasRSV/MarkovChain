module Main where

import MarkovChains.FirstChain
import MarkovChains.Mchain

import System.Environment

import System.IO as I
import System.Directory as D
import Data.Map as M
import Control.Monad
import Control.Monad.State


{-haveAchat :: IO ()-}
{-haveAchat = do -}
               {-file <- readFile memoryname -}
               {-transit converse (read file)-}
               {-return ()-}



{-makeSureMemoryExist :: IO ()-}
{-makeSureMemoryExist = do-}
                       {-fe <- D.doesFileExist memoryFileName-}


                       {-unless fe $ do-}
                                  {-print "Creating File"-}
                                  {-handle <- I.openFile memoryname I.WriteMode-}

                                  {-I.hPrint handle (([], M.empty) :: ([String], M.Map String [String]))-}
                                  {-I.hClose handle-}


{-Don't judge the filepath-}
memoryFileName :: FilePath
memoryFileName = "/Users/jonval/WARNING/singularity/PATH/.memories/chain.mem"

makeSureMemoryExistMchain :: IO ()
makeSureMemoryExistMchain = 
  do
     fe <- D.doesFileExist memoryFileName


     unless fe $ do
                print "Creating File"
                handle <- I.openFile memoryFileName I.WriteMode

                I.hPrint handle (M.singleton "Awesome" ["Bot"] :: M.Map String [String])
                I.hClose handle

                  


chainAction :: Mchain String ->  IO ()
chainAction ch =
  do
    handle <- I.openFile memoryFileName I.ReadMode
    state <- I.hGetContents handle
    (answer, state') <- runStateT ch (read state)
    putStrLn answer

    {-For some Reason the previous handle is closed by running the previous IO action-}
    {-I don't know why, would love to know why..-}

    handle' <- I.openFile memoryFileName I.WriteMode
    I.hPrint handle' state'
    I.hFlush handle'
    I.hClose handle'


chat :: IO ()
chat = forever $ chainAction demoRespondLine



main :: IO ()
main = do 
          makeSureMemoryExistMchain

          args <- getArgs
          case args of 
            [] -> putStr "Learn or Chat & Learn with\n-c to chat linewise\n-l to learn bulk"
            ("-c":_) -> chat
            ("-l":_) -> chainAction bulkLearn


