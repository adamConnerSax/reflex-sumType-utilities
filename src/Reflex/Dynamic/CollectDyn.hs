{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DataKinds                  #-}
module Reflex.Dynamic.CollectDyn where

import Generics.SOP (NP,SListI,hmap,I(I), (:.:)(Comp))
import Generics.SOP.DMapUtilities (npSequenceViaDMap)
import Reflex (Reflex,Dynamic,distributeDMapOverDynPure)

{-
-- | Convert a datastructure whose constituent parts are all 'Dynamic's into a
-- single 'Dynamic' whose value represents all the current values of the input's
-- consitutent 'Dynamic's.
collectDynPure :: ( RebuildSortedHList (HListElems b)
                  , IsHList a, IsHList b
                  , AllAreFunctors (Dynamic t) (HListElems b)
                  , Reflex t
                  , HListElems a ~ FunctorList (Dynamic t) (HListElems b)
                  ) => a -> Dynamic t b
collectDynPure ds = fmap fromHList $ distributeFHListOverDynPure $ toFHList $ toHList ds
-}

type family Equals (a :: k) (b :: k) :: Bool where
   Equals a a = True
   Equals a b = False

type family IsFunctorWrapped (f :: * -> *)  (x :: *)  (fx :: *) :: Bool where
  IsFunctorWrapped f x y = Equals (f x) y

type family IsFunctorWrappedList (f :: * -> *) (xs :: [*]) (fxs :: [*]) :: Bool where
  IsFunctorWrappedList f '[] '[] = True
  IsFunctorWrappedList f (x ': xs') '[] = False
  IsFunctorWrappedList f '[] (fx ': fxs') = False
  IsFunctorWrappedList f (x ': xs') (fx ': fxs') = (IsFunctorWrapped f x fx) && (IsFunctorWrappedList f xs' fxs')
  

--collectDynPure::(Generic a, Generic b, ??)::a -> Dynamic t b

collectDynPureNP::(Reflex t, SListI xs)=>NP (Dynamic t) xs -> Dynamic t (NP I xs)
collectDynPureNP = npSequenceViaDMap distributeDMapOverDynPure . hmap (Comp . fmap I) 
  
