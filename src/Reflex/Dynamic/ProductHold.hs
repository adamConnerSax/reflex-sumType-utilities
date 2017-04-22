{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE RankNTypes                 #-}
module Reflex.Dynamic.ProductHold where

import           Data.Functor.Compose        (Compose (Compose,getCompose))
import           Control.Monad               (join)

import           Generics.SOP                (Code, Generic,
                                              All2, (:.:)(Comp), unComp, 
                                              hsequence, hcliftA, hmap, POP, unPOP, 
                                              SListI,SListI2,  hcollapse, I, Proxy(..),hcmap, And, hliftA2, type (-.->)(..)
                                             ,hap, hpure, hcpure)
import           Generics.SOP.NP             (NP, sequence'_NP, sequence_NP, cpure_POP)
import           Generics.SOP.Dict           (Dict(..),withDict,zipAll2,unAll_POP)

import           Generics.SOP.PerConstructor (functorToPerConstructorList
                                             , functorDoPerConstructorWithNames
                                             , constructorNameList
                                             , MapFieldsAndSequence
                                             , NatAt)
import           Generics.SOP.DMapUtilities (npSequenceViaDMap)
import           Generics.SOP.Distribute

import           Reflex                      (Dynamic, Event, Reflex
                                             , fmapMaybe, leftmost, updated
                                             , distributeDMapOverDynPure, uniqDyn)

import Reflex.Dynamic.PerConstructor (DynMaybe,DynMBuildable(..),AllDynMBuildable)


joinMaybesAndUniq::(Eq a, Reflex t)=> (DynMaybe t :.: Maybe) a -> DynMaybe t a
joinMaybesAndUniq = Compose . uniqDyn . fmap join . getCompose . unComp

reComposeDynMaybe::Reflex t=>DynMaybe t a -> (Dynamic t :.: Maybe) a
reComposeDynMaybe = Comp . getCompose

widgetWithUniq::(Reflex t, Eq a, Functor m, DynMBuildable t m a) => (DynMaybe t :.: Maybe) a -> (m :.: Dynamic t :.: Maybe) a
widgetWithUniq = Comp . fmap reComposeDynMaybe . dynMBuild . joinMaybesAndUniq

widgetWithUniq'::(Reflex t, Functor m, (And Eq (DynMBuildable t m)) a) => (DynMaybe t :.: Maybe) a -> (m :.: Dynamic t :.: Maybe) a
widgetWithUniq' = widgetWithUniq

doSequencing::(Reflex t, SListI xs, Applicative m)=>NP (m :.: Dynamic t :.: Maybe) xs -> (m :.: Dynamic t :.: Maybe) (NP I xs)
doSequencing = Comp . fmap (Comp . fmap hsequence . npSequenceViaDMap distributeDMapOverDynPure) . sequence'_NP

reCompose::Functor m=>(m :.: Dynamic t :.: Maybe) a -> m (DynMaybe t a)
reCompose = fmap (Compose . unComp) . unComp 

functionPOPFromClass::forall c f g xss.(SListI2 xss)=>Dict (All2 c) xss->(forall a.c a=>f a -> g a)->POP (f -.-> g) xss
functionPOPFromClass d fn = withDict d $ cpure_POP (Proxy :: Proxy c) $ Fn fn

-- This is safe in the sense that if you give it a sum-type, you get back a widget per constructor instead of ignoring 
buildSafeEqProduct::forall a t m.(Generic a
                                 , All2 Eq (Code a)
                                 , Reflex t
                                 , Applicative m)
  =>POP (DynMaybe t -.-> m :.: DynMaybe t) (Code a) -> DynMaybe t a->[m (DynMaybe t a)]
buildSafeEqProduct buildFns =
  let slistIC = Proxy :: Proxy SListI
      eqC = Proxy :: Proxy Eq
--      hmapWidgetAndUniq::POP (DynMaybe t :.: Maybe) (Code a) -> POP (m :.: Dynamic t :.: Maybe) (Code a)
      hmapWidgetAndUniq = hmap (Comp . fmap (Comp . getCompose) . unComp) . hap buildFns . hcmap eqC joinMaybesAndUniq
  in fmap reCompose . hcollapse . reconstructA . hcmap slistIC (Comp . doSequencing) . unPOP . hmapWidgetAndUniq . distributeToFields . reAssociateNP . functorToNP


buildSafeDynMBuildableEqProduct::forall a t m.(Generic a
                                              , All2 Eq (Code a)
                                              , AllDynMBuildable t m a
                                              , Reflex t
                                              , Applicative m)
  =>DynMaybe t a->[m (DynMaybe t a)]
buildSafeDynMBuildableEqProduct = buildSafeEqProduct makeDynMBuildPOP

-- NB: This assumes that the input has only one constructor.  It does not check!
-- but we know that any type has at least one constructor.  So it should never crash on an empty list
buildUnsafeDynMBuildableEqProduct::(Generic a
                      , All2 Eq (Code a)
                      , AllDynMBuildable t m a
                      , Reflex t
                      , Applicative m)
  =>DynMaybe t a->m (DynMaybe t a)
buildUnsafeDynMBuildableEqProduct = head . buildSafeDynMBuildableEqProduct



makeDynMBuildPOP::forall xss t m.(SListI2 xss,Reflex t, All2 (DynMBuildable t m) xss, Functor m)=>POP (DynMaybe t -.-> (m :.: DynMaybe t)) xss
makeDynMBuildPOP = functionPOPFromClass (Dict :: Dict (All2 (DynMBuildable t m)) xss) (Comp . dynMBuild)



