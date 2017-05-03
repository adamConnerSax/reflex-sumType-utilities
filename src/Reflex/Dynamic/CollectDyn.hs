{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Reflex.Dynamic.CollectDyn
  (
    distributeNPOverDyn
  , collectDynGeneric
  )where

import           Generics.SOP               ((:.:) (Comp), Code, Generic, I (I),
                                             NP, NS, Proxy (..), SListI,
                                             SListI2, SOP (..), from, hcliftA,
                                             hliftA, hmap, hsequence', to,
                                             unComp, unI, unSOP)

import           Generics.SOP.DMapUtilities (FunctorWrapTypeList,
                                             FunctorWrapTypeListOfLists,
                                             npReCompose, npSequenceViaDMap,
                                             nsOfnpReCompose)

import           Reflex                     (Dynamic, Reflex,
                                             distributeDMapOverDynPure)



distributeNPOverDyn::(Reflex t, SListI xs)=>NP I (FunctorWrapTypeList (Dynamic t) xs) -> Dynamic t (NP I xs)
distributeNPOverDyn = collectDynPureNP . hliftA (unI . unComp) . npReCompose

collectDynGeneric::(Reflex t,Generic a, Generic b, (Code a) ~ FunctorWrapTypeListOfLists (Dynamic t) (Code b))=>a -> Dynamic t b
collectDynGeneric = fmap (to . SOP) . hsequence' . collectDynPureNSNP . aToNSNPI

aToNSNPI::(Generic a, Code a ~ FunctorWrapTypeListOfLists (Dynamic t) xss, SListI2 xss) =>a -> NS (NP (I :.: Dynamic t)) xss
aToNSNPI = nsOfnpReCompose . unSOP . from

collectDynPureNSNP::(Reflex t,SListI2 xss)=>NS (NP (I :.: Dynamic t)) xss -> NS (Dynamic t :.: NP I) xss
collectDynPureNSNP =
  let slistIC = Proxy :: Proxy SListI
  in hcliftA slistIC (Comp . collectDynPureNP . hliftA (unI . unComp))

collectDynPureNP::(Reflex t, SListI xs)=>NP (Dynamic t) xs -> Dynamic t (NP I xs)
collectDynPureNP = npSequenceViaDMap distributeDMapOverDynPure . hliftA (Comp . fmap I)

