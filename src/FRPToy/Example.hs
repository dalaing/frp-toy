{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE RecursiveDo #-}
module FRPToy.Example (
    example
  ) where

import Text.Read (readMaybe)

import Control.Monad (forever)
import Control.Monad.Trans (liftIO)
import Control.Concurrent (forkIO)

import FRPToy.Moment
import FRPToy.Event
import FRPToy.Behavior
import FRPToy.Run

example :: Moment ()
example = do
  (eLine, fireLine) <- newEventSource
  liftIO . forkIO . forever $ do
    x <- getLine
    fireLine x

  bLine <- hold "" eLine

  let
    eInt = fmapMaybe readMaybe eLine
    eFizz = "Fizz" <$ ffilter (\x -> x `mod` 3 == 0) eInt
    eBuzz = "Buzz" <$ ffilter (\x -> x `mod` 5 == 0) eInt
    eFizzBuzz = mergeWith (++) eFizz eBuzz

  bFizzBuzz <- hold "" eFizzBuzz

  let
    eSwitch1 =
      (* 3) <$> eInt
    eSwitch2 =
      (* 5) <$> eInt

  beSwitch <- hold never $
              mergeWith const (eSwitch1 <$ eFizz) (eSwitch2 <$ eBuzz)

  rec
    bCount <- hold 0 $ (+ 1) <$> tag bCount eLine

  let bBoth = (,) <$> bCount <*> bFizzBuzz

  reactimate putStrLn eFizzBuzz
  reactimate print (tag bBoth eLine)
  reactimate print (switch beSwitch)
