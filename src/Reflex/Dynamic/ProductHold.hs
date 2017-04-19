{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ConstraintKinds            #-}
module Reflex.Dynamic.ProductHold where

import           Data.Functor.Compose        (Compose (Compose,getCompose))

import           Generics.SOP                (HasDatatypeInfo, Code, Generic, ConstructorName,
                                              All2, (:.:)(..), unComp, 
                                              hsequence, hcliftA, hmap, unPOP, 
                                              SListI, Proxy (Proxy),I)
import           Generics.SOP.NP             (NP, sequence'_NP, sequence_NP)


import           Generics.SOP.PerConstructor (functorToPerConstructorList
                                             , functorDoPerConstructorWithNames
                                             , constructorNameList
                                             , MapFieldsAndSequence
                                             , NatAt)
import           Generics.SOP.DMapUtilities (npSequenceViaDMap)

import           Reflex                      (Dynamic, Event, Reflex
                                             , fmapMaybe, leftmost, updated, distributeDMapOverDynPure)

import Reflex.Dynamic.PerConstructor (DynMBuildable,AllDynMBuildable)


{-
holdEqProduct::(Generic a, All2 Eq (Code a), AllDynMBuildable t m a)=>DynMaybe t a->m (DynMaybe t a)
holdEqProduct = distributeToFields . functorToNP
-}
