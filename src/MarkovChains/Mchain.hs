{-# LANGUAGE FlexibleInstances #-}
module MarkovChains.Mchain (Mchain, listenForGroups, demoTalkLineWise, demoGenerateNGroups, demoGetRandomState, bulkLearnGroups) where


import Control.Monad.State
import System.Random as R
import Control.Monad.Identity
import Data.Map as M
import Data.Maybe


type S = (M.Map String [String], Maybe String)
type Mchain = StateT S IO


doWhileAtMost :: Monad m => (a -> Bool) -> Int -> m a -> m [a]
doWhileAtMost _ 0 _ = return []
doWhileAtMost c i m = do 
                         t <- m

                         if c t
                          then (:) <$> return t <*> doWhileAtMost c (i - 1) m
                          else return []

concatatN :: Int -> [String] -> [String]
concatatN _ [] = []
concatatN i l = Prelude.foldr (\a b -> if b == "" then a else a ++ " " ++ b) "" (take i l) : concatatN i (drop i l)

pickPath :: [String] -> IO String
pickPath options = (options !!) <$> R.randomRIO (0, length options - 1) 

mappend' :: Maybe String -> Maybe String -> Maybe String
mappend' (Just a) (Just b) = Just (a ++ " " ++  b)
mappend' (Just a) Nothing = Just a
mappend' Nothing (Just a) = Just a
mappend' Nothing Nothing = Nothing

outputInc :: Mchain (Maybe String)
outputInc = do
              (m, p) <- get 

              case p >>= (`M.lookup` m) of
                Nothing -> put (m, mempty) >> return mempty
                Just ts -> do
                              word <- liftIO (pickPath ts)
                              put (m, Just word)
                              return $ Just word

updateState :: M.Map String [String] -> [String] -> (M.Map String [String], Maybe String)
updateState m [] = (m, Just "You Created a Wierd Chain or Well No Input Probably")
updateState m [a] = (m, Just a)
updateState m (p:n:nx) = if M.member p m
                           then updateState (M.adjust (n:) p m) (n:nx)
                           else updateState (M.insert p [n] m) (n:nx)

listenForGroups :: Int -> IO String -> Mchain (Maybe String)
listenForGroups groupsz ioAction = 
  do
    (m, _) <- get
    ws <- liftIO (concatatN groupsz . words <$> ioAction)
    let (m', ls) = updateState m ws
    put (m', ls) 
    return ls


bulkLearnGroups :: Int -> Mchain String
bulkLearnGroups groupsz = do
                word <- listenForGroups groupsz getContents
                return . fromJust $ (++) <$> pure "Learned up to " <*> word 

demoSentenceLength :: IO Int
demoSentenceLength = R.randomRIO (7, 20)

demoGenerateNGroups :: Int -> Mchain String
demoGenerateNGroups groups = do 
                                 sentence <- doWhileAtMost (/= Nothing) groups outputInc 
                                 case sentence of
                                  (word@Just{} : sentence') -> return . fromJust $ Prelude.foldr mappend' word sentence' 
                                  _ -> liftIO (print "I might be stuck in a loop") >> demoGetRandomState >> demoGenerateNGroups groups

demoGetRandomState :: Mchain String
demoGetRandomState = do 
                        (m, _) <- get
                        let states = Prelude.map fst $ M.toList m 
                        idx <- liftIO $ R.randomRIO (0, length states - 1)
                        put (m, Just $ states !! idx)
                        return $ states !! idx

demoTalkLineWise :: Int -> Mchain String
demoTalkLineWise groupsz = do
                       _ <- listenForGroups groupsz getLine
                       l <- liftIO demoSentenceLength
                       sentence <- doWhileAtMost (/= Nothing) l outputInc

                       case sentence of
                        (word@Just{} : sentence') -> return . fromJust $ Prelude.foldr mappend' word sentence' 
                        _ -> demoGetRandomState >> demoGenerateNGroups l
                       




 













              


              




                                 







