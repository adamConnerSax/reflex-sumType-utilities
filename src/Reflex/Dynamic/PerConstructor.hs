{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ConstraintKinds            #-}
module Reflex.Dynamic.PerConstructor
  (
    DynMaybe
  , dynamicToEventList
  , dynamicToNamedEventList
  , dynMaybeToEventList
  , dynMaybeToNamedEventList
  , whichFired
  , ConWidget(..)
  , DynMBuildable(..)
  , AllDynMBuildable
  , dynamicToConWidgets
  , dynamicToWidgetEvent
  , dynMaybeToConWidgets
  , dynMaybeToWidgetEvent
  , dynMBuildableToConWidgets
  , dynMBuildableToWidgetEvent
  , Generic
  , HasDatatypeInfo
  ) where

import           Control.Monad               (join)
import           Data.Functor.Compose        (Compose (Compose,getCompose))

import           Generics.SOP                (HasDatatypeInfo, Code, Generic, ConstructorName,
                                              All2, (:.:)(..), unComp, 
                                              hsequence, hcliftA, hmap, unPOP, 
                                              SListI, Proxy (Proxy))
import           Generics.SOP.NP             (NP, sequence'_NP, sequence_NP)


import           Generics.SOP.PerConstructor (functorToPerConstructorList
                                             , functorDoPerConstructorWithNames
                                             , constructorNameList
                                             , MapFieldsAndSequence
                                             , NatAt)

import           Reflex                      (Dynamic, Event, Reflex
                                             , fmapMaybe, leftmost, updated)


-- pure

type DynMaybe t = Compose (Dynamic t) Maybe

dynamicToEventList::(Reflex t, Generic a)=>Dynamic t a -> [Event t a]
dynamicToEventList = functorToPerConstructorList (hmap dynMaybeToEvent)

dynamicToNamedEventList::forall t a.(Reflex t, HasDatatypeInfo a, Generic a)=>Dynamic t a -> [(ConstructorName,Event t a)]
dynamicToNamedEventList dynA =
  let names = constructorNameList (Proxy :: Proxy a)
  in zip names (dynamicToEventList dynA)

dynMaybeToEvent::forall a t (f :: k -> *).Reflex t=>((Dynamic t :.: Maybe) :.: f) a -> (Event t :.: f) a
dynMaybeToEvent = Comp . fmapMaybe id . updated . unComp . unComp

dynMaybeToEventList::(Reflex t, Generic a)=>DynMaybe t a -> [Event t a]
dynMaybeToEventList = functorToPerConstructorList (hmap dynMaybeMaybeToEvent)

dynMaybeToNamedEventList::forall t a.(Reflex t, HasDatatypeInfo a, Generic a)=>DynMaybe t a -> [(ConstructorName,Event t a)]
dynMaybeToNamedEventList dynA =
  let names = constructorNameList (Proxy :: Proxy a)
  in zip names (dynMaybeToEventList dynA)

dynMaybeMaybeToEvent::forall a t (f :: k -> *).Reflex t=>((DynMaybe t :.: Maybe) :.: f) a -> (Event t :.: f) a
dynMaybeMaybeToEvent = Comp . fmapMaybe join . updated . getCompose . unComp . unComp

whichFired::Reflex t=>[Event t a]->Event t Int
whichFired = leftmost . zipWith (<$) [0..]

-- now for widget building

data ConWidget t m a = ConWidget { conName::ConstructorName, switchedTo::Event t a, widget::m (DynMaybe t a) }

class DynMBuildable t m a where
  dynMBuild::DynMaybe t a -> m (DynMaybe t a)

type AllDynMBuildable t m a = (All2 (DynMBuildable t m) (Code a))

type MapAndSequenceDynMaybeFields t m a = MapFieldsAndSequence (DynMaybe t) (Compose m (DynMaybe t)) (Code a)

dynamicToConWidgets::(Reflex t, Generic a,HasDatatypeInfo a, Applicative m)
  =>MapAndSequenceDynMaybeFields t m a->Dynamic t a-> [ConWidget t m a]
dynamicToConWidgets mapAndS dynA =
  let switchEvents = dynamicToEventList dynA
      mapFsAndS = mapAndS . hmap (Compose . unComp)
      namedWidgets = functorDoPerConstructorWithNames mapFsAndS dynA
  in zipWith (\ev (n,w) -> ConWidget n ev (getCompose w)) switchEvents namedWidgets

joinComposedMaybes::Reflex t=>(DynMaybe t :.: Maybe) a -> (Dynamic t :.: Maybe) a
joinComposedMaybes = Comp . fmap join . getCompose . unComp

dynMaybeToConWidgets::(Reflex t, Generic a,HasDatatypeInfo a, Applicative m)
  =>MapAndSequenceDynMaybeFields t m a->DynMaybe t a-> [ConWidget t m a]
dynMaybeToConWidgets mapAndS dynMA =
  let switchEvents = dynMaybeToEventList dynMA
      mapFsAndS = mapAndS . hmap (Compose . unComp)
      namedWidgets = functorDoPerConstructorWithNames (mapFsAndS . hmap joinComposedMaybes) dynMA
  in zipWith (\ev (n,w) -> ConWidget n ev (getCompose w)) switchEvents namedWidgets


dynamicToWidgetEvent::(Reflex t, Generic a,HasDatatypeInfo a, Applicative m)
  =>MapAndSequenceDynMaybeFields t m a->Dynamic t a->Event t (m (DynMaybe t a))
dynamicToWidgetEvent mapAndS dynA =
  let conWidgets = dynamicToConWidgets mapAndS dynA
      f (ConWidget _ ev w) = w <$ ev
  in leftmost $ f <$> conWidgets

dynMaybeToWidgetEvent::(Reflex t, Generic a,HasDatatypeInfo a, Applicative m)
  =>MapAndSequenceDynMaybeFields t m a->DynMaybe t a->Event t (m (DynMaybe t a))
dynMaybeToWidgetEvent mapAndS dynMA =
  let conWidgets = dynMaybeToConWidgets mapAndS dynMA
      f (ConWidget _ ev w) = w <$ ev
  in leftmost $ f <$> conWidgets


-- specialized to the DynMBuildable case
dynMBuildableToConWidgets::forall t m a.( Reflex t
                                        , Generic a
                                        , HasDatatypeInfo a
                                        , Applicative m
                                        , AllDynMBuildable t m a)
  =>DynMaybe t a -> [ConWidget t m a]
dynMBuildableToConWidgets = dynMaybeToConWidgets widgetFieldsAndSequence where
  widgetFieldsAndSequence =
    let buildC = Proxy :: Proxy (DynMBuildable t m)
        sListIC = Proxy :: Proxy SListI
    in hcliftA sListIC (Comp . sequence_NP) . unPOP . hcliftA buildC (Compose . dynMBuild)



dynMBuildableToWidgetEvent::(Reflex t, Generic a,HasDatatypeInfo a, Applicative m,AllDynMBuildable t m a)
  =>DynMaybe t a->Event t (m (DynMaybe t a))
dynMBuildableToWidgetEvent dynMA =
  let conWidgets = dynMBuildableToConWidgets dynMA
      f (ConWidget _ ev w) = w <$ ev
  in leftmost $ f <$> conWidgets
  
