{-# LANGUAGE FlexibleInstances #-}
module MarkovChains.Mchain (Mchain, bulkLearn, demoRespondLine) where


import Control.Monad.State
import System.Random as R
import Control.Monad.Identity
import Data.Map as M
import Data.Maybe


type S = M.Map String [String]
type Mchain = StateT S IO


bindWhile :: (Monad f, Eq a) => (a -> Bool) -> (a -> f a) -> a -> f [a]
bindWhile f bind p = do
                      p' <- bind p

                      if f p 
                        then (:) <$> bind p <*> bindWhile f bind p'
                        else return []


bindWhileMax :: (Monad f, Eq a) => (a -> Bool) -> (a -> f a) -> Int -> a -> f [a]
bindWhileMax _ _ 0 _ = return []
bindWhileMax f bind n p = do
                             p' <- bind p
                             (:) <$> bind p <*> bindWhileMax f bind (n - 1) p'


mappend' :: Maybe String -> Maybe String -> Maybe String
mappend' (Just a) (Just b) = Just (a ++ " " ++  b)
mappend' (Just a) Nothing = Just a
mappend' Nothing (Just a) = Just a
mappend' Nothing Nothing = Nothing

outputInc :: Maybe String -> Mchain (Maybe String)
outputInc p = do
              s <- get 

              case p >>= (`M.lookup` s) of
                Nothing -> return mempty
                Just ts -> do
                              idx <- liftIO $ R.randomRIO (0, length ts - 1)
                              return $ Just (ts !! idx)

updateState :: M.Map String [String] -> [String] -> (M.Map String [String], String)
updateState m [] = (m, "You Created a Wierd Chain or Well No Input Probably")
updateState m [a] = (m, a)
updateState m (p:n:nx) = if M.member p m
                           then updateState (M.adjust (n:) p m) (n:nx)
                           else updateState (M.insert p [n] m) (n:nx)

listen :: IO String -> Mchain String
listen ioAction = 
  do
    m <- get
    ws <- liftIO (words <$> ioAction)
    let (m', ls) = updateState m ws
    put m'
    return ls



bulkLearn :: Mchain String
bulkLearn = do
                word <- listen getContents
                return $ "Learned up to " ++ word


demoBW :: Int -> Maybe String -> Mchain [Maybe String]
demoBW = bindWhileMax (/= Nothing) outputInc

demoSentenceLength :: IO Int
demoSentenceLength = R.randomRIO (7, 20)

fromRandomStarter :: Mchain String
fromRandomStarter = do
                       memory <- get

                       let keys = Prelude.map fst $ M.toList memory
                       idx <- liftIO (R.randomRIO (0, length keys - 1))
                       sl <- liftIO demoSentenceLength
                       let word = Just (keys !! idx)
                       words <- demoBW sl word

                       return . fromJust $ Prelude.foldl mappend' word words

demoRespondLine :: Mchain String
demoRespondLine = do
                    word <- listen getLine
                    
                    sl <- liftIO demoSentenceLength
                    (word' : words) <- demoBW sl (Just word)
                    if isNothing word'
                      then fromRandomStarter
                      else return . fromJust $ Prelude.foldl mappend' word' words











              


              




                                 







