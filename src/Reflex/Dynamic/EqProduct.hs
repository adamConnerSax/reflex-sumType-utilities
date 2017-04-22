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
  , functionPOPFromClass
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

import Reflex.Dynamic.PerConstructor (DynMaybe,DynMBuildable(..),AllDynMBuildable)

joinMaybesAndUniq::(Eq a, Reflex t)=> (DynMaybe t :.: Maybe) a -> DynMaybe t a
joinMaybesAndUniq = Compose . uniqDyn . fmap join . getCompose . unComp

doSequencing::(Reflex t, SListI xs, Applicative m)=>NP (m :.: Dynamic t :.: Maybe) xs -> (m :.: Dynamic t :.: Maybe) (NP I xs)
doSequencing = Comp . fmap (Comp . fmap hsequence . npSequenceViaDMap distributeDMapOverDynPure) . hsequence' --sequence'_NP

allCompToMDynMaybe::Functor m=>(m :.: Dynamic t :.: Maybe) a -> m (DynMaybe t a)
allCompToMDynMaybe = fmap (Compose . unComp) . unComp 

compDynMaybeToAllComp::Functor m=>(m :.: DynMaybe t) a -> (m :.: Dynamic t :.: Maybe) a
compDynMaybeToAllComp = Comp . fmap (Comp . getCompose) . unComp

functionPOPFromClass::forall c f g xss.SListI2 xss=>Dict (All2 c) xss->(forall a.c a=>f a -> g a)->POP (f -.-> g) xss
functionPOPFromClass d fn = withDict d $ hcpure (Proxy :: Proxy c) $ Fn fn

-- This is safe in the sense that if you give it a sum-type, you get back a widget per constructor instead of ignoring
-- hmapWidgetAndUniq seems..unoptimal.  3 applications over the POP.
buildSafeEqProduct::forall a t m.(Generic a, All2 Eq (Code a), Reflex t, Applicative m)
  =>POP (DynMaybe t -.-> m :.: DynMaybe t) (Code a) -> DynMaybe t a->[m (DynMaybe t a)]
buildSafeEqProduct buildFns =
  let slistIC = Proxy :: Proxy SListI
      eqC = Proxy :: Proxy Eq
      dynMaybeToPOP = distributeToFields . reAssociateNP . functorToNP
      uniqAndBuild = hmap compDynMaybeToAllComp . hap buildFns . hcmap eqC joinMaybesAndUniq
      popToListOfBuilt = fmap allCompToMDynMaybe . hcollapse . reconstructA . hcmap slistIC (Comp . doSequencing) . unPOP
  in  popToListOfBuilt . uniqAndBuild . dynMaybeToPOP


buildSafeDynMBuildableEqProduct::forall a t m.(Generic a
                                              , All2 Eq (Code a)
                                              , All2 (DynMBuildable t m) (Code a)
                                              , Reflex t
                                              , Applicative m)
  =>DynMaybe t a->[m (DynMaybe t a)]
buildSafeDynMBuildableEqProduct = buildSafeEqProduct makeDynMBuildPOP

makeDynMBuildPOP::forall xss t m.(SListI2 xss,Reflex t, All2 (DynMBuildable t m) xss, Functor m)=>POP (DynMaybe t -.-> (m :.: DynMaybe t)) xss
makeDynMBuildPOP = functionPOPFromClass (Dict :: Dict (All2 (DynMBuildable t m)) xss) (Comp . dynMBuild)


-- NB: This assumes that the input has only one constructor.  It does not check!
-- but we know that any type has at least one constructor.  So it should never crash on an empty list
buildUnsafeDynMBuildableEqProduct::(Generic a
                      , All2 Eq (Code a)
                      , AllDynMBuildable t m a
                      , Reflex t
                      , Applicative m)
  =>DynMaybe t a->m (DynMaybe t a)
buildUnsafeDynMBuildableEqProduct = head . buildSafeDynMBuildableEqProduct





