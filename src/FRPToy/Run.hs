{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
module FRPToy.Run (
    run
  ) where

import Control.Applicative ((<|>))
import Control.Monad (forM, forM_, void, forever)
import Control.Monad.Fix (MonadFix(..))
import Data.Functor.Identity (Identity(..))

import Control.Lens

import Control.Monad.State (StateT, runStateT, execStateT, modify)
import Control.Monad.Reader (ReaderT, runReaderT, asks)
import Control.Monad.Trans (MonadIO(..), lift)

import Control.Monad.Primitive
import Data.Unique.Tag
import Data.Dependent.Sum
import Data.Dependent.Map

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Concurrent.STM.TMVar

import FRPToy.Types

type TagMap = DMap (Tag (PrimState IO))

data EventSource a = EventSource (a -> IO ()) (TMVar a)
data EventSink a = EventSink (a -> IO ()) (Event a)

data NetworkState =
  NetworkState {
    _nsEventSources    :: TagMap EventSource
  , _nsEventSinks      :: TagMap EventSink
  , _nsBehaviorInitial :: TagMap Identity
  , _nsBehaviorHold    :: TagMap Event
  }

makeLenses ''NetworkState

initialNetworkState :: NetworkState
initialNetworkState =
  NetworkState empty empty empty empty

type MonadMoment = StateT NetworkState IO

data FrameState =
  FrameState {
    _fsEventState    :: TagMap Maybe
  , _fsBehaviorState :: TagMap Identity
  }

makeLenses ''FrameState

initialFrameState :: FrameState
initialFrameState =
  FrameState empty empty

type ReadFrame = ReaderT FrameState IO
type WriteFrame = StateT (TagMap Identity) (ReaderT FrameState IO)

mkEventSource :: IO (Tag (PrimState IO) a, EventSource a)
mkEventSource = do
  t <- newTag
  v <- atomically newEmptyTMVar
  let f = atomically . putTMVar v
  pure (t, EventSource f v)

readEventSource :: EventSource a -> STM (Maybe a)
readEventSource (EventSource _ tm) =
  tryTakeTMVar tm

readEventSources :: NetworkState -> IO (TagMap Maybe)
readEventSources ns =
  liftIO . atomically $ traverseWithKey (const readEventSource) (ns ^. nsEventSources)

runEventSink :: EventSink a -> ReadFrame ()
runEventSink (EventSink f e) = do
  me <- runEvent e
  liftIO . forM_ me $ f

runEventSinks :: NetworkState -> ReadFrame (TagMap Maybe)
runEventSinks ns =
  traverseWithKey (\_ -> fmap (const Nothing) . runEventSink) (ns ^. nsEventSinks)

runMoment :: Moment a -> MonadMoment a
runMoment (MFmap f m) =
  f <$> runMoment m
runMoment (MPure a) =
  pure a
runMoment (MAp f x) =
  runMoment f <*> runMoment x
runMoment (MBind x f) =
  runMoment x >>= runMoment . f
runMoment (MFix f) =
  mfix (runMoment . f)
runMoment (MLiftIO io) =
  liftIO io
runMoment MEventSource = do
  (t, es@(EventSource fire _)) <- liftIO mkEventSource
  nsEventSources %= insert t es
  pure (ESource t, fire)
runMoment (MReactimate f e) = do
  t <- liftIO newTag
  nsEventSinks %= insert t (EventSink f e)
  pure ()
runMoment (MHold a e) = do
  t <- lift newTag
  nsBehaviorInitial %= insert t (Identity a)
  nsBehaviorHold %= insert t e
  pure $ BHold t e

runEvent :: Event a -> ReadFrame (Maybe a)
runEvent ENever =
  pure Nothing
runEvent (ESource t) =
  asks $ (! t) . view fsEventState
runEvent (EFmapMaybe f e) = do
  me <- runEvent e
  pure $ me >>= f
runEvent (EMergeWith f e1 e2) = do
  me1 <- runEvent e1
  me2 <- runEvent e2
  pure $ f <$> me1 <*> me2 <|> me1 <|> me2
runEvent (EAttachWithMaybe f b e) = do
  bv <- readBehavior b
  me <- runEvent e
  pure $ me >>= f bv
runEvent (ESwitch be) = do
  e <- readBehavior be
  runEvent e

readBehavior :: Behavior a -> ReadFrame a
readBehavior (BPure a) =
  pure a
readBehavior (BFmap f a) =
  f <$> readBehavior a
readBehavior (BAp f x) =
  readBehavior f <*> readBehavior x
readBehavior (BHold t _) =
  fmap runIdentity . asks $ (! t) . view fsBehaviorState

writeHold :: Tag (PrimState IO) a -> Event a -> WriteFrame (Maybe a)
writeHold t e = do
  me <- lift $ runEvent e
  forM_ me $ \a ->
    modify (insert t (Identity a))
  pure me

writePhase :: NetworkState -> WriteFrame ()
writePhase ns =
  void . traverseWithKey writeHold $ ns ^. nsBehaviorHold

runMonadMoment :: MonadMoment a -> IO a
runMonadMoment m = do
  (a, ns) <- runStateT m initialNetworkState

  let
    ibFrame = ns ^. nsBehaviorInitial
    loop bFrame = do
      eFrame  <- readEventSources ns
      flip runReaderT (FrameState eFrame bFrame) $
        runEventSinks ns
      bFrame' <- flip runReaderT (FrameState eFrame bFrame) .
                 flip execStateT bFrame $
                 writePhase ns
      loop bFrame'

  loop ibFrame

  pure a

run :: Moment a -> IO a
run = runMonadMoment . runMoment
