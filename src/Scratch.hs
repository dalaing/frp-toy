{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}
module Scratch where

import Control.Applicative ((<|>))
import Control.Monad (forM, forM_, void, forever)
import Data.Functor.Identity (Identity(..))
import Text.Read (readMaybe)
import Text.Show

import Data.Proxy (Proxy(..))
import GHC.Exts (Constraint)

import Data.Functor.Classes

import Control.Lens

import Control.Monad.State (StateT, runStateT, execStateT, modify)
import Control.Monad.Reader (ReaderT, runReaderT, asks)
import Control.Monad.Trans (lift, MonadIO, liftIO)
import Control.Monad.Fix (MonadFix(..))

import Data.Distributive
import Control.Comonad.Cofree

import Control.Monad.Primitive
import Data.Unique.Tag
import Data.Dependent.Sum
import Data.Dependent.Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar

import Data.Constraint
import Control.Exception
import System.IO.Unsafe

{-# NOINLINE nextNodeIdRef #-}
nextNodeIdRef :: IORef Int
nextNodeIdRef = unsafePerformIO $ newIORef 1

newNodeId :: IO Int
newNodeId = atomicModifyIORef' nextNodeIdRef $ \n -> (succ n, n)

{-# NOINLINE unsafeNodeId #-}
unsafeNodeId :: a -> Int
unsafeNodeId a = unsafePerformIO $ do
  touch a
  newNodeId

data NodeInfo f =
  NodeInfo {
    niId :: !Int
  , niUpstream  :: !IntSet
  , niDownstream  :: !(f IntSet)
  }

instance Eq1 f => Eq (NodeInfo f) where
  NodeInfo i1 us1 ds1 == NodeInfo i2 us2 ds2 =
    i1 == i2 && us1 == us2 && eq1 ds1 ds2

instance Show1 f => Show (NodeInfo f) where
  showsPrec n (NodeInfo i us ds) =
    showString "NodeInfo" . showParen True (showsPrec n i . showString "," . showsPrec n us . showString "," . showsPrec1 n ds)

readNodeInfo :: NodeInfo IORef -> IO (NodeInfo Identity)
readNodeInfo (NodeInfo i us ds) = do
  ds' <- readIORef ds
  pure $! NodeInfo i us (Identity ds')

{-# NOINLINE mkNodeInfo #-}
mkNodeInfo :: a -> [NodeInfo IORef] -> NodeInfo IORef
mkNodeInfo a ns = unsafePerformIO $ do
  touch a
  i <- newNodeId
  ds <- newIORef IntSet.empty
  forM_ ns $ \n ->
    atomicModifyIORef' (niDownstream n) (\is -> let is' = IntSet.insert i is in (is', is'))
  pure $! NodeInfo i (IntSet.fromList . fmap niId $ ns) ds

class HasAnn k where
  getAnn :: k -> [NodeInfo IORef]

-- Thanks (?) to
-- https://gist.github.com/jkarni/0872c83bc4264a23fec1
-- via
-- https://gist.github.com/Icelandjack/5afdaa32f41adf3204ef9025d9da2a70
getAnn' :: a -> [NodeInfo IORef]
getAnn' y = unsafePerformIO $ do
    res <- try $ evaluate $ getAnn'' Dict y
    case res of
      Right x -> return x
      Left (TypeError _) -> return []

getAnn'' :: (Dict (HasAnn a)) -> a -> [NodeInfo IORef]
getAnn'' Dict x = getAnn x

data EventF (f :: * -> *) a where
  ENever :: NodeInfo f -> EventF f a
  ESource :: NodeInfo f -> Tag (PrimState IO) a -> EventF f a
  EFmapMaybe :: NodeInfo f -> (a -> Maybe b) -> EventF f a -> EventF f b
  EMergeWith :: NodeInfo f -> (a -> a -> a) -> EventF f a -> EventF f a -> EventF f a
  EAttachWithMaybe :: NodeInfo f -> (a -> b -> Maybe c) -> BehaviorF f a -> EventF f b -> EventF f c
  ESwitch :: NodeInfo f -> BehaviorF f (NR f (EventF f a)) -> EventF f a

type Event = EventF IORef

instance Functor (EventF IORef) where
  fmap f = fmapMaybe (Just . f)

never :: Event a
never = ENever (mkNodeInfo () [])

source :: Tag (PrimState IO) a -> Event a
source t = ESource (mkNodeInfo t []) t

fmapMaybe :: (a -> Maybe b) -> Event a -> Event b
fmapMaybe f e = EFmapMaybe (mkNodeInfo e (getAnn e)) f e

ffilter :: (a -> Bool) -> Event a -> Event a
ffilter p = fmapMaybe (\x -> if p x then Just x else Nothing)

mergeWith :: (a -> a -> a) -> Event a -> Event a -> Event a
mergeWith f e1 e2 = EMergeWith (mkNodeInfo (e1, e2) (getAnn e1 ++ getAnn e2)) f e1 e2

attachWithMaybe :: (a -> b -> Maybe c) -> Behavior a -> Event b -> Event c
attachWithMaybe f b e = EAttachWithMaybe (mkNodeInfo (b, e) (getAnn b ++ getAnn e)) f b e

switch :: Behavior (NR IORef (Event a)) -> Event a
switch be = ESwitch (mkNodeInfo be (getAnn be)) be

instance HasAnn (EventF IORef a) where
  getAnn (ENever a) = pure a
  getAnn (ESource a _) = pure a
  getAnn (EFmapMaybe a _ _) = pure a
  getAnn (EMergeWith a _ _ _) = pure a
  getAnn (EAttachWithMaybe a _ _ _) = pure a
  getAnn (ESwitch a _) = pure a

data BehaviorF (f :: * -> *) a where
  BPure :: NodeInfo f -> a -> BehaviorF f a
  BFmap :: NodeInfo f -> (a -> b) -> BehaviorF f a  -> BehaviorF f b
  BAp :: NodeInfo f -> BehaviorF f (a -> b) -> BehaviorF f a -> BehaviorF f b
  BHold :: NodeInfo f -> a -> EventF f a -> BehaviorF f a

type Behavior = BehaviorF IORef

instance Functor (BehaviorF IORef) where
  fmap f b = BFmap (mkNodeInfo b (getAnn b)) f b

instance Applicative (BehaviorF IORef) where
  pure x = BPure (mkNodeInfo x (getAnn' x)) x
  (<*>) b1 b2 = BAp (mkNodeInfo (b1, b2) (getAnn b1 ++ getAnn b2)) b1 b2

hold :: a -> Event a -> Behavior a
hold i e = BHold (mkNodeInfo (i, e) (getAnn' i ++ getAnn e)) i e

instance HasAnn (BehaviorF IORef a) where
  getAnn (BPure a _) = pure a
  getAnn (BFmap a _ _) = pure a
  getAnn (BAp a _ _) = pure a
  getAnn (BHold a _ _) = pure a

boom =
  let
    eInt = never
    eFizz = "Fizz" <$ ffilter (\x -> x `mod` 3 == 0) eInt
    eBuzz = "Buzz" <$ ffilter (\x -> x `mod` 5 == 0) eInt
    eSwitch1 = (* 3) <$> eInt
    eSwitch2 = (* 5) <$> eInt
    beSwitch = hold (mkNR never) .
               fmap (fmap ($ False)) $
               mergeWith const
                 (distribute (\n -> mkNR eSwitch1) <$ eFizz)
                 (distribute (\n -> mkNR eSwitch2) <$ eBuzz)
  in
    switch beSwitch

newtype Ref a = Ref { unRef :: a }
  deriving (Eq, Show)

instance Functor Ref where
  fmap f (Ref x) = Ref (f x)

instance Foldable Ref where
  foldMap f (Ref x) = f x

instance Traversable Ref where
  traverse f (Ref x) = Ref <$> f x

instance Applicative Ref where
  pure = Ref
  Ref f <*> Ref x = Ref (f x)

instance Monad Ref where
  Ref x >>= f = f x

instance Distributive Ref where
  distribute = Ref . fmap unRef

data NodeRef f a = NodeRef (NodeInfo f) a
  deriving (Eq, Show)

instance HasAnn (Ref (NodeRef IORef a)) where
  getAnn (Ref (NodeRef n _)) = [n]

instance Functor (NodeRef f) where
  fmap f (NodeRef n x) = NodeRef n (f x)

readNodeRef :: NodeRef IORef a -> IO (NodeRef Identity a)
readNodeRef (NodeRef n a) = do
  n' <- readNodeInfo n
  pure $! NodeRef n' a

type NR f a = Ref (NodeRef f a)

readNR :: NR IORef a -> IO (NR Identity a)
readNR = fmap Ref . readNodeRef . unRef

mkNR :: a -> NR IORef a
mkNR x = Ref . NodeRef (mkNodeInfo x []) $ x

asdf x =
  fmap ($ x) .
  distribute $ \n ->
    if even n
    then distribute [mkNR 1, mkNR 2]
    else distribute [mkNR 3, mkNR 4]


-- what happens when we have Ref [Event a] and we use fmap leftmost to get to Ref (Event a)?
-- I think that is fine - the result event will have references to the input events

{-
switch is going to be a pain
we'll need a functor over BehaviorF, and then we have 
  BFmap :: (a -> b) -> BehaviorF ann a -> BehaviorF ann b
or the like to fmap over, which is tough when a ~ EventF ann a

Could work with Thing ty ann a where ty = EventTy | BehaviorTy?
Could do some type family magic 
-}


{-
-- could use cofree for this, and can drop back down to fix when done
data EventF ann a where
  ENever :: ann -> EventF ann a
  ESource :: ann -> Tag (PrimState IO) a -> EventF ann a
  EFmapMaybe :: ann -> (a -> Maybe b) -> EventF ann a -> EventF ann b
  EMergeWith :: ann -> (a -> a -> a) -> EventF ann a -> EventF ann a -> EventF ann a
  EAttachWithMaybe :: ann -> (a -> b -> Maybe c) -> BehaviorF ann a -> EventF ann b -> EventF ann c
  ESwitch :: ann -> BehaviorF ann (EventF ann a) -> EventF ann a

eventAnn :: EventF ann a -> ann
eventAnn (ENever a) = a
eventAnn (ESource a _) = a
eventAnn (EFmapMaybe a _ _) = a
eventAnn (EMergeWith a _ _ _) = a
eventAnn (ESwitch a _) = a

type Event = EventF (NodeInfo IORef)

instance Functor Event where
  fmap f e = EFmapMaybe (mkNodeInfo e [eventAnn e]) (Just . f) e

type Behavior = BehaviorF (NodeInfo IORef)

data BehaviorF ann a where
  BPure :: ann -> a -> BehaviorF ann a
  BFmap :: ann -> (a -> b) -> BehaviorF ann a -> BehaviorF ann b
  BAp :: ann -> Behavior (a -> b) -> BehaviorF ann a -> BehaviorF ann b
  BHold :: ann -> Tag (PrimState IO) a -> EventF ann a -> BehaviorF ann a

behaviorAnn :: BehaviorF ann a -> ann
behaviorAnn (BPure a _) = a
behaviorAnn (BFmap a _ _) = a
behaviorAnn (BAp a _ _) = a
behaviorAnn (BHold a _ _) = a

instance Functor Behavior where
  fmap f b = BFmap (mkNodeInfo b [behaviorAnn b]) f b

instance Applicative Behavior where
  pure x = BPure (mkNodeInfo x []) x
  (<*>) f x = BAp (mkNodeInfo (f,x) [behaviorAnn f, behaviorAnn x]) f x
-}
