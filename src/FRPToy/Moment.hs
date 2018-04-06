{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module FRPToy.Moment (
    Moment
  , newEventSource
  , reactimate
  , hold
  ) where

import FRPToy.Types

newEventSource :: Moment (Event a, a -> IO ())
newEventSource = MEventSource

reactimate :: (a -> IO ()) -> Event a -> Moment ()
reactimate = MReactimate

hold :: a -> Event a -> Moment (Behavior a)
hold = MHold
