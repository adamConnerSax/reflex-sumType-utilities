{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE UndecidableInstances       #-}
module Reflex.Dynamic.CollectDyn where

import Generics.SOP (NP,SListI,hmap,I(I),unI, (:.:)(Comp),unComp,from,to, Generic,Code,SOP(..),unSOP,hsequence,hliftA)
import Generics.SOP.DMapUtilities (npSequenceViaDMap,npRecompose)
import Reflex (Reflex,Dynamic,distributeDMapOverDynPure)

import Data.Type.Bool (type (&&))

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

type family FunctorWrapList (f :: * -> *) (xs :: [*]) :: [*] where
  FunctorWrapList f '[] = '[]
  FunctorWrapList f (x ': xs') = f x ': FunctorWrapList f xs'

type family FunctorWrapListOfLists f (xss :: [[*]]) :: [[*]] where
  FunctorWrapListOfLists f '[] = '[]
  FunctorWrapListOfLists f (xs ': xss') = FunctorWrapList f xs ': FunctorWrapListOfLists f xss' 

collectDynPure::(Generic a, Generic b, (Code a) ~ FunctorWrapListOfLists (Dynamic t) (Code b))=>a -> Dynamic t b
collectDynPure = fmap (to . SOP) . hsequence . hliftA (Comp . collectDynPureNP . hliftA (fmap unI . unComp) . npRecompose) . unSOP . from a

collectDynPureNP::(Reflex t, SListI xs)=>NP (Dynamic t) xs -> Dynamic t (NP I xs)
collectDynPureNP = npSequenceViaDMap distributeDMapOverDynPure . hmap (Comp . fmap I) 
  
--

type family Equals (a :: k) (b :: k) :: Bool where
   Equals a a = True
   Equals a b = False

type family IsFunctorWrappedList (f :: * -> *) (xs :: [*]) (fxs :: [*]) :: Bool where
  IsFunctorWrappedList f '[] '[] = True
  IsFunctorWrappedList f (x ': xs') '[] = False
  IsFunctorWrappedList f '[] (fx ': fxs') = False
  IsFunctorWrappedList f (x ': xs') (fx ': fxs') = (Equals (f x) fx) && (IsFunctorWrappedList f xs' fxs')

  
type family IsFunctorWrappedListOfLists (f :: * -> *) (xss :: [[*]]) (fxss :: [[*]]) :: Bool where
  IsFunctorWrappedListOfLists f '[] '[] = True
  IsFunctorWrappedListOfLists f (xs ': xss') '[] = False
  IsFunctorWrappedListOfLists f '[] (fxs ': fxss') = False
  IsFunctorWrappedListOfLists f (xs ': xss') (fxs ': fxss') = (IsFunctorWrappedList f xs fxs) && IsFunctorWrappedListOfLists f xss' fxss' 
