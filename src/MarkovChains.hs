{-# LANGUAGE MultiParamTypeClasses #-}
module MarkovChains
    (Chain,
     transit,
     MarkovChain,
     learn,
     think,
     say,
     listen,
     converse,
     memoryname,
     option,
     trythen
    ) where

import Data.Map as M
import System.Random as R
import Data.List as L

newtype Chain s v = Chain { transit :: s -> IO (v, s) }

instance Functor (Chain s) where
  fmap f (Chain f') = Chain (\s -> do 
                                      (v, s') <- f' s 
                                      return (f v, s'))

instance Applicative (Chain s) where
  pure x = Chain (\s -> pure (x, s))
  (Chain t) <*> (Chain t') = Chain (\s -> do 
                                            (f, s') <- t s 
                                            (v, s'') <- t' s'
                                            return (f v, s''))


instance Monad (Chain a) where
  return = pure
  (Chain t) >>= f =  Chain (\s -> do 
                                    (a, s') <- t s 
                                    transit (f a) s')


type MarkovChain = Chain ([String], M.Map String [String]) [String] 

{-Atomic Thingy-}
nextstate :: [String] -> IO String
nextstate [] = return ""
nextstate options = 
  do
    opt <- R.randomRIO (0, length options - 1) :: IO Int

    return (options !! opt)



{-Logical Operators-}

option :: MarkovChain -> MarkovChain -> MarkovChain
option ch1 ch2 = Chain $ \state -> 
  do
    resp <- transit ch1 state

    case resp of 
      ([], ([], _)) -> transit ch2 state
      resp' -> return resp'


trythen :: MarkovChain -> MarkovChain -> MarkovChain
trythen ch1 ch2 = Chain $ \state ->
  do
    resp <- transit ch1 state

    case resp of 
      ([], ([], _)) -> transit failure state
      (_, state') -> transit ch2 state'


failure :: MarkovChain 
failure = Chain $ \(_, mp) -> pure ([], ([], mp))


{-Conversation Starters -}

clueless :: MarkovChain
clueless = Chain $ \(_, mp) ->
  let defaultgreeting = ["Hello"]
    in pure (defaultgreeting, (defaultgreeting, mp))


vaugeidea :: MarkovChain
vaugeidea = Chain $ \state@(_, mp) ->
  let keys' = Prelude.map fst . M.toList $ mp
    in case keys' of 
      [] -> transit failure state
      _ -> do
            opt <- R.randomRIO (0, length keys' - 1) :: IO Int
            return ([keys' !! opt], ([keys' !! opt], mp))


casanova :: MarkovChain
casanova = Chain $ \state@(ws, mp) ->
  case ws of
    [] -> transit failure state
    [w] -> do
              (nw:_, _)  <- transit (build `option` vaugeidea `option` clueless) state
              return ([nw], ([nw], mp))

    _ -> transit failure state



{-StateUpdaters-}

build :: MarkovChain
build = Chain $ \state@(ws, mp) ->
    case ws of 
      [] -> transit failure state
      (w:ws) ->
        case M.lookup w mp of
          Nothing -> transit failure state
          Just chances -> 
              do 
                w' <- nextstate chances
                return (w': w: ws, (w': w: ws, mp))


remember :: MarkovChain
remember = Chain $ \state@(ws, mp) ->
  case ws of
    (pw:cw:nws) -> let mp' = if M.member pw mp
                         then M.adjust (cw:) pw mp
                         else M.insert pw [cw] mp
                    in return (cw:nws, (cw:nws, mp'))

    l@[lw] -> transit failure (l, mp) 
    [] -> transit failure ([], mp)




talkable :: MarkovChain
talkable =  Chain $ \state@(ws, mp) ->
  case ws of 
    [] -> transit (vaugeidea `option` clueless) state
    [w] -> transit (casanova `option` vaugeidea `option` clueless) state
    _ -> transit failure  state




{-IO-}

listen :: MarkovChain
listen = Chain $ \(_, mp) ->
  do 
    contents <- getContents
    return (words contents, (words contents, mp))

say :: MarkovChain
say = Chain $ \state@(ws, mp) -> 
  if length ws < speechcap
    then transit failure state
    else do
          print . unwords . reverse $ ws
          return (ws, ([], mp))


blurt :: MarkovChain
blurt = Chain $ \state@(ws, mp) -> 
    do
      print . unwords . reverse $ ws
      return (ws, ([], mp))




{-High Level Thingies-}

learn :: MarkovChain
learn = talkable `option` do 
                            remember 
                            learn
    
think :: MarkovChain
think = say `option` (build `trythen` think) `option` blurt



persist :: MarkovChain 
persist = Chain $ \state ->
  do 
    writeFile memoryname . show $ state

    return ([], state)



converse :: MarkovChain
converse = do
            listen
            learn
            think
            say
            persist


speechcap :: Int
speechcap = 50 


{-Recommend adding a absolutePath here-}
memoryname :: FilePath
memoryname = "chain.mem"









