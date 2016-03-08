module Main where

import Control.Exception
import Control.Monad.STM
import Data.Typeable
import Control.Concurrent.STM.TVar

data AnimalException = AnimalException deriving (Show, Typeable)
instance Exception AnimalException where
data BigamyException = BigamyException deriving (Show, Typeable)
instance Exception BigamyException where

data Person = Person
  { name :: String
  , age :: Int
  , mate :: Maybe (TVar Person)
  }

marry :: TVar Person -> TVar Person -> IO ()
marry a b = atomically $ do
  aStatus <- readTVar a
  bStatus <- readTVar b
  case mate aStatus of
    Nothing -> pure ()
    Just _ -> throwSTM BigamyException
  case mate bStatus of
    Nothing -> pure ()
    Just _ -> throwSTM BigamyException
  writeTVar a aStatus{ mate = Just b }
  writeTVar b bStatus{ mate = Just a }

allocatePerson :: String -> Int -> IO (TVar Person)
allocatePerson name age = atomically $ do
  me <- newTVar (Person name age Nothing)
  alwaysSucceeds $ do
    Person _ myAge myMate <- readTVar me
    case myMate of
      Nothing -> pure ()
      Just _ | myAge >= 16 -> pure ()
             | otherwise -> throwSTM AnimalException
  pure me

main = do
  girl <- allocatePerson "loli" 9
  s3p <- allocatePerson "s3p" 30
  marry s3p girl
