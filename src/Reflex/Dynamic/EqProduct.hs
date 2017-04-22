{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE RankNTypes                 #-}
module Reflex.Dynamic.EqProduct
  (
    buildSafeEqProduct
  , buildSafeEqProductFromConstraint
  , buildSafeDynMBuildableEqProduct
  , buildUnsafeDynMBuildableEqProduct
  ) where

import           Data.Functor.Compose        (Compose (Compose,getCompose))
import           Control.Monad               (join)

import           Generics.SOP                (Code, Generic
                                             , All2, SListI, SListI2, I
                                             , (:.:)(Comp), unComp
                                             , hmap, hcmap, hliftA2 
                                             , hsequence, hsequence'
                                             , hap, hcpure, hcollapse
                                             , NP, POP, unPOP
                                             , Proxy(..), type (-.->)(..))               

import           Generics.SOP.Dict           (Dict(..),withDict)


import           Generics.SOP.DMapUtilities (npSequenceViaDMap)
import           Generics.SOP.Distribute (functorToNP,reAssociateNP,distributeToFields,reconstructA)

import           Reflex                      (Dynamic, Event, Reflex, distributeDMapOverDynPure, uniqDyn)

import Reflex.Dynamic.Common (DynMaybe,DynMBuildable(..),AllDynMBuildable,functionPOPFromClass)

joinMaybesAndUniq::(Eq a, Reflex t)=> (DynMaybe t :.: Maybe) a -> DynMaybe t a
joinMaybesAndUniq = Compose . uniqDyn . fmap join . getCompose . unComp

doSequencing::(Reflex t, SListI xs, Applicative m)=>NP (m :.: Dynamic t :.: Maybe) xs -> (m :.: Dynamic t :.: Maybe) (NP I xs)
doSequencing = Comp . fmap (Comp . fmap hsequence . npSequenceViaDMap distributeDMapOverDynPure) . hsequence' --sequence'_NP

allCompToMDynMaybe::Functor m=>(m :.: Dynamic t :.: Maybe) a -> m (DynMaybe t a)
allCompToMDynMaybe = fmap (Compose . unComp) . unComp 

compDynMaybeToAllComp::Functor m=>(m :.: DynMaybe t) a -> (m :.: Dynamic t :.: Maybe) a
compDynMaybeToAllComp = Comp . fmap (Comp . getCompose) . unComp


-- This is safe in the sense that if you give it a sum-type, you get back a widget per constructor instead of ignoring
-- uniqAndBuild seems..unoptimal.  3 applications over the POP. Can they be fused?
buildSafeEqProduct::forall a t m.(Generic a, All2 Eq (Code a), Reflex t, Applicative m)
  =>POP (DynMaybe t -.-> m :.: DynMaybe t) (Code a) -> DynMaybe t a->[m (DynMaybe t a)]
buildSafeEqProduct buildFns =
  let slistIC = Proxy :: Proxy SListI
      eqC = Proxy :: Proxy Eq
      dynMaybeToPOP = distributeToFields . reAssociateNP . functorToNP
      uniqAndBuild = hmap compDynMaybeToAllComp . hap buildFns . hcmap eqC joinMaybesAndUniq
      popToListOfBuilt = fmap allCompToMDynMaybe . hcollapse . reconstructA . hcmap slistIC (Comp . doSequencing) . unPOP
  in  popToListOfBuilt . uniqAndBuild . dynMaybeToPOP

buildSafeEqProductFromConstraint::forall c a t m.(Generic a, All2 Eq (Code a), Reflex t, Applicative m)
  =>Dict (All2 c) (Code a)
  ->(forall a.c a=>DynMaybe t a -> (m :.: DynMaybe t) a)
  -> DynMaybe t a
  -> [m (DynMaybe t a)]
buildSafeEqProductFromConstraint d fn = buildSafeEqProduct (functionPOPFromClass d fn)

-- special case for DynMBuildable. Also serves as an example for how to use the constraint machinery.

buildSafeDynMBuildableEqProduct::forall a t m.(Generic a
                                              , All2 Eq (Code a)
                                              , All2 (DynMBuildable t m) (Code a)
                                              , Reflex t
                                              , Applicative m)
  =>DynMaybe t a->[m (DynMaybe t a)]
buildSafeDynMBuildableEqProduct = buildSafeEqProductFromConstraint (Dict :: Dict (All2 (DynMBuildable t m)) (Code a)) (Comp . dynMBuild)

-- NB: This assumes that the input has only one constructor.  It does not check!
-- but we know that any type has at least one constructor.  So it should never crash on an empty list
buildUnsafeDynMBuildableEqProduct::(Generic a
                                   , All2 Eq (Code a)
                                   , All2 (DynMBuildable t m) (Code a)
                                   , Reflex t
                                   , Applicative m)
  =>DynMaybe t a->m (DynMaybe t a)
buildUnsafeDynMBuildableEqProduct = head . buildSafeDynMBuildableEqProduct





