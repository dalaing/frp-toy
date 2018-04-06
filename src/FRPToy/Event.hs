{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module FRPToy.Event (
    Event
  , never
  , fmapMaybe
  , ffilter
  , mergeWith
  , attachWithMaybe
  , attachWith
  , tag
  , switch
  ) where

import FRPToy.Types

never :: Event a
never = ENever

fmapMaybe :: (a -> Maybe b) -> Event a -> Event b
fmapMaybe = EFmapMaybe

ffilter :: (a -> Bool) -> Event a -> Event a
ffilter p = EFmapMaybe (\x -> if p x then Just x else Nothing)

mergeWith :: (a -> a -> a) -> Event a -> Event a -> Event a
mergeWith = EMergeWith

attachWithMaybe :: (a -> b -> Maybe c) -> Behavior a -> Event b -> Event c
attachWithMaybe = EAttachWithMaybe

attachWith :: (a -> b -> c) -> Behavior a -> Event b -> Event c
attachWith f = EAttachWithMaybe (\a -> Just . f a)

tag :: Behavior a -> Event b -> Event a
tag = EAttachWithMaybe (const . Just)

switch :: Behavior (Event a) -> Event a
switch = ESwitch
