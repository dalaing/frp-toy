{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE GADTs #-}
module FRPToy.Types (
    Event(..)
  , Behavior(..)
  , Moment(..)
  ) where

import Control.Monad.Trans (MonadIO(..))
import Control.Monad.Fix (MonadFix(..))

import Control.Monad.Primitive
import Data.Unique.Tag

data Event a where
  ENever :: Event a
  ESource :: Tag (PrimState IO) a -> Event a
  EFmapMaybe :: (a -> Maybe b) -> Event a -> Event b
  EMergeWith :: (a -> a -> a) -> Event a -> Event a -> Event a
  EAttachWithMaybe :: (a -> b -> Maybe c) -> Behavior a -> Event b -> Event c
  ESwitch :: Behavior (Event a) -> Event a

instance Functor Event where
  fmap f = EFmapMaybe (Just . f)

data Behavior a where
  BPure :: a -> Behavior a
  BFmap :: (a -> b) -> Behavior a -> Behavior b
  BAp :: Behavior (a -> b) -> Behavior a -> Behavior b
  BHold :: Tag (PrimState IO) a -> Event a -> Behavior a

instance Functor Behavior where
  fmap = BFmap

instance Applicative Behavior where
  pure = BPure
  (<*>) = BAp

data Moment a where
  MFmap :: (a -> b) -> Moment a -> Moment b
  MPure :: a -> Moment a
  MAp :: Moment (a -> b) -> Moment a -> Moment b
  MBind :: Moment a -> (a -> Moment b) -> Moment b
  MFix :: (a -> Moment a) -> Moment a
  MLiftIO :: IO a -> Moment a
  MEventSource :: Moment (Event a, a -> IO ())
  MReactimate :: (a -> IO ()) -> Event a -> Moment ()
  MHold :: a -> Event a -> Moment (Behavior a)

instance Functor Moment where
  fmap = MFmap

instance Applicative Moment where
  pure = MPure
  (<*>) = MAp

instance Monad Moment where
  (>>=) = MBind

instance MonadFix Moment where
  mfix = MFix

instance MonadIO Moment where
  liftIO = MLiftIO
