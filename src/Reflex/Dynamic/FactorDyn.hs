{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reflex.Dynamic.FactorDyn where

import           Generics.SOP.Distribute
import           Generics.SOP.DMapUtilities
import           Generics.SOP.PerConstructor

import Reflex.Dynamic.PerConstructor

import           Generics.SOP
import           Reflex

import           Control.Monad.Fix
import qualified Data.List as L
import Data.Maybe (isJust)

-- | b has the same structure as a but all its fields have a Dynamic t of whatever a had in that field
--
-- E.g, Maybe a and Maybe (Dynamic t a) or Either x y  and Either (Dynamic t x) (Dynamic t y)
--
-- NB: The fields don't all have to be parameters of the type.
-- data A a = A1 Int | A2 a and data B t a = B1 (Dynamic t Int) | B2 (Dynamic t a)
dynamicOfSumToNestedEvs :: (Reflex t, Generic a 
                           , MonadHold t m) => Dynamic t a -> m (Event t (Event t a))
dynamicOfSumToNestedEvs da = do
  a0 <- sample $ current da
  let evs = dynamicToEventList da
      which0 = (\(Just x) -> x) . L.findIndex isJust $ unI . unComp <$> functorToPerConstructorList id (I a0)
      whichEv = whichFired evs
  currentIndex <- holdDyn which0 whichEv -- Dynamic t Int, where the int tells you which constructor last fired.
  let newEv = attachWithMaybe (\cI newI -> if cI == newI then Nothing else Just ()) (current currentIndex) whichEv 
      aEv = leftmost evs
  return $ aEv <$ newEv
