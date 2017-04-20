{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ConstraintKinds            #-}
module Reflex.Dynamic.ProductHold where

import           Data.Functor.Compose        (Compose (Compose,getCompose))
import           Control.Monad               (join)

import           Generics.SOP                (Code, Generic,
                                              All2, (:.:)(..), unComp, 
                                              hsequence, hcliftA, hmap, POP, unPOP, 
                                              SListI, hcollapse, I, Proxy(..),hcmap, And,hliftA2)
import           Generics.SOP.NP             (NP, sequence'_NP, sequence_NP)
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


joinMaybes::Reflex t=> (DynMaybe t :.: Maybe) a -> DynMaybe t a
joinMaybes = Compose . fmap join . getCompose . unComp

reComposeAndUniqDynMaybe::(Reflex t, Eq a)=>DynMaybe t a -> (Dynamic t :.: Maybe) a
reComposeAndUniqDynMaybe = Comp . uniqDyn . getCompose

widgetAndUniq::(Eq a, Reflex t, Functor m, DynMBuildable t m a) => (DynMaybe t :.: Maybe) a -> (m :.: Dynamic t :.: Maybe) a
widgetAndUniq = Comp . fmap reComposeAndUniqDynMaybe . dynMBuild . joinMaybes 

widgetAndUniq'::(Reflex t, Functor m, (And Eq (DynMBuildable t m)) a) => (DynMaybe t :.: Maybe) a -> (m :.: Dynamic t :.: Maybe) a
widgetAndUniq' = widgetAndUniq

doSequencing::(Reflex t, SListI xs, Applicative m)=>NP (m :.: Dynamic t :.: Maybe) xs -> (m :.: Dynamic t :.: Maybe) (NP I xs)
doSequencing = Comp . fmap (Comp . fmap hsequence . npSequenceViaDMap distributeDMapOverDynPure) . sequence'_NP

reCompose::Functor m=>(m :.: Dynamic t :.: Maybe) a -> m (DynMaybe t a)
reCompose = fmap (Compose . unComp) . unComp 

-- This is safe in the sense that if you give it a sum-type, you get back a widget per constructor instead of ignoring 
-- There is a lot of constraint massaging in there.  I think there's an easier way but I tried a few things...
safeEqProduct::forall a t m.(Generic a
                            , All2 Eq (Code a)
                            , AllDynMBuildable t m a
                            , Reflex t
                            , Applicative m)
  =>DynMaybe t a->[m (DynMaybe t a)]
safeEqProduct =
  let slistIC = Proxy :: Proxy SListI
      eqDict = Dict :: Dict (All2 Eq) (Code a)
      dmbDict = Dict :: Dict (All2 (DynMBuildable t m)) (Code a)
      eqAnddmbDict = zipAll2 eqDict dmbDict
      eqAnddmbPOP = unAll_POP eqAnddmbDict
      hmapWidgetAndUniq::POP (DynMaybe t :.: Maybe) (Code a) -> POP (m :.: Dynamic t :.: Maybe) (Code a)
      hmapWidgetAndUniq = hliftA2  (\c x -> withDict c widgetAndUniq' x) eqAnddmbPOP
  in fmap reCompose . hcollapse . reconstructA . hcmap slistIC (Comp . doSequencing) . unPOP . hmapWidgetAndUniq . distributeToFields . reAssociateNP . functorToNP

-- NB: This assumes that the input has only one constructor.  It does not check!
-- but we know that any type has at least one constructor.  So it should never crash on an empty list
unsafeEqProduct::(Generic a
                 , All2 Eq (Code a)
                 , AllDynMBuildable t m a
                 , Reflex t
                 , Applicative m)
  =>DynMaybe t a->m (DynMaybe t a)
unsafeEqProduct = head . safeEqProduct
