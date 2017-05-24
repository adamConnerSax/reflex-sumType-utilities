{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# LANGUAGE DeriveGeneric         #-}

{-|
Module: Reflex.FanGeneric
Description: Generic (generics-sop) implementation of fan and select
-}
module Reflex.FanGeneric
  (
    EventSelectorGeneric (..)
  , selectGenericUnary
  , fanGeneric
  , makeProductOfAllTypeListTags
  , Generic
  , Code
  ) where

import           Generics.SOP               ((:.:) (Comp), All2, Code, Generic,
                                             I (I), NP (..), NS (Z), Proxy (..),
                                             SListI, SListI2, SOP (..), from,
                                             hcliftA, hliftA, hsequence', to,
                                             unComp, unI, unSOP)

import           Generics.SOP.DMapUtilities (FunctorWrapTypeList,
                                             FunctorWrapTypeListOfLists,
                                             TypeListConstructs,
                                             TypeListTag (..),
                                             makeProductOfAllTypeListTags,
                                             makeTypeListTagNP, npReCompose,
                                             npSequenceViaDMap, npUnCompose,
                                             nsOfnpReCompose, selectTypedFromNP)

import           Generics.SOP.Distribute    (functorToNP)

import           Reflex.Class               (Event, EventSelector (..), Reflex,
                                             constDyn, fan, fmapMaybe, updated)
import           Reflex.Dynamic             (Dynamic, distributeDMapOverDynPure)

import           Generics.SOP.Instances     ()
import qualified GHC.Generics               as GHCG

newtype EventSelectorGeneric t xss  = EventSelectorGeneric
  {
    selectGeneric :: forall a tla. (Reflex t, SListI2 xss, SListI tla, Generic a, (Code a) ~ TypeListConstructs tla) => TypeListTag xss tla -> Event t a
  }

data UnaryHelper a = UnaryHelper { getUnary :: a } deriving (GHCG.Generic)
instance Generic (UnaryHelper a)

selectGenericUnary :: (Reflex t, SListI2 xss, SListI tla,  (Code (UnaryHelper a)) ~ TypeListConstructs tla)
  => EventSelectorGeneric t xss -> TypeListTag xss tla -> Event t a
selectGenericUnary esg  = fmap getUnary . selectGeneric esg

fanGeneric :: forall t a. (Reflex t, Generic a) => Event t a -> EventSelectorGeneric t (Code a)
fanGeneric ev =
  let sListIC = Proxy :: Proxy SListI
      npOfEvents :: NP (Event t :.: NP I) (Code a)
      npOfEvents = hcliftA sListIC (Comp . fmapMaybe id . fmap unComp . unComp) $ functorToNP ev
  in EventSelectorGeneric $ \tag -> selectTypedFromNP npOfEvents tag

{-
selectTypedFromNP :: (Functor g, Generic a, Code a ~ Constructs xs, SListI xs, SListI2 xss) => NP (g :.: NP I) xss -> TypeListTag xss xs -> g a
selectTypedFromNP np tag = to . SOP . Z <$> selectFromNP np tag

type family Constructs (xs :: [k])  ::  [[k]] where
  Constructs x = x ': '[]
--  Constructs '[] = '[]
--  Constructs (x ': '[]) = Code x
--  Constructs (x ': y ': xs) = (x ': y ': xs) ': '[]

selectFromNP :: forall g xss xs. (Functor g, SListI2 xss, SListI xs) => NP (g :.: NP I) xss -> TypeListTag xss xs -> g (NP I xs)
selectFromNP np tag = go np tag
  where
    go :: NP (g :.: NP I) yss -> TypeListTag yss ys -> g (NP I ys)
    go Nil _ = error "Reached the end of typelist before the tag was satified."
    go (gy :* _) TLHead = unComp gy
    go (_ :* npTail) (TLTail tailTag) = go npTail tailTag
-}

-- for example
data FanExample = FEA | FEB | FEC C | FED Int Double deriving (GHCG.Generic)
instance Generic FanExample

data C = C1 Int Int | C2 Double Double deriving (GHCG.Generic)
instance Generic C

exampleFan :: Reflex t => EventSelectorGeneric t (Code FanExample)
exampleFan = fanGeneric (updated $ constDyn $ FEA)

(aTag, bTag, cTag, dTag) = makeProductOfAllTypeListTags (Proxy :: Proxy FanExample)

evNullaryA :: Reflex t => Event t ()
evNullaryA = selectGeneric exampleFan aTag

evNullaryB :: Reflex t => Event t ()
evNullaryB = selectGeneric exampleFan bTag

evC :: Reflex t => Event t C
evC = selectGenericUnary exampleFan cTag

evD :: Reflex t => Event t (Int,Double)
evD = selectGeneric exampleFan dTag

data DLike = DLike Int Double deriving (GHCG.Generic)
instance Generic DLike

evDLike :: Reflex t => Event t DLike
evDLike = selectGeneric exampleFan dTag


