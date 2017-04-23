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

import Generics.SOP               (NS, NP,SListI, SListI2, hmap,I(I),unI
                                  , (:.:)(Comp),unComp,from,to, Generic
                                  ,Code,SOP(..),unSOP
                                  ,hsequence',hliftA, hcliftA, Proxy(..))
       
import Generics.SOP.DMapUtilities (npSequenceViaDMap,npRecompose,nsOfnpRecompose
                                  ,FunctorWrapTypeList,FunctorWrapTypeListOfLists)
       
import Reflex                     (Reflex,Dynamic,distributeDMapOverDynPure)



distributeNPOverDyn::(Reflex t, SListI xs)=>NP I (FunctorWrapTypeList (Dynamic t) xs) -> Dynamic t (NP I xs)
distributeNPOverDyn = collectDynPureNP . hliftA (unI . unComp) . npRecompose

collectDynPure::(Reflex t,Generic a, Generic b, (Code a) ~ FunctorWrapTypeListOfLists (Dynamic t) (Code b))=>a -> Dynamic t b
collectDynPure = fmap (to . SOP) . hsequence' . collectDynPureNSNP . aToNSNPI

aToNSNPI::(Generic a, Code a ~ FunctorWrapTypeListOfLists (Dynamic t) xss, SListI2 xss) =>a -> NS (NP (I :.: Dynamic t)) xss
aToNSNPI = nsOfnpRecompose . unSOP . from

collectDynPureNSNP::(Reflex t,SListI2 xss)=>NS (NP (I :.: Dynamic t)) xss -> NS (Dynamic t :.: NP I) xss
collectDynPureNSNP =
  let slistIC = Proxy :: Proxy SListI
  in hcliftA slistIC (Comp . collectDynPureNP . hliftA (unI . unComp))

collectDynPureNP::(Reflex t, SListI xs)=>NP (Dynamic t) xs -> Dynamic t (NP I xs)
collectDynPureNP = npSequenceViaDMap distributeDMapOverDynPure . hliftA (Comp . fmap I) 

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



{-
-- | Collect a hetereogeneous list whose elements are all 'Dynamic's into a
-- single 'Dynamic' whose value represents the current values of all of the
-- input 'Dynamic's.
distributeFHListOverDynPure :: (Reflex t, RebuildSortedHList l) => FHList (Dynamic t) l -> Dynamic t (HList l)
distributeFHListOverDynPure l = fmap dmapToHList $ distributeDMapOverDynPure $ fhlistToDMap l
-}

