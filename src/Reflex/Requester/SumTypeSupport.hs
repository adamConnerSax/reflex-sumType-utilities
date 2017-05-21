{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-|
Module: Reflex.Requester.SumTypeSupport
Description: Aeson ToJSON and FromJSON instances for the TypeListTag type and DSum for use in websocket communication
-}
module Reflex.Requester.SumTypeSupport
  (
  ) where

import qualified Data.Dependent.Sum         as DS
import           Generics.SOP               (I (..), NP (..), NS (..),
                                             SList (..), SListI (..),
                                             Shape (..), hapInjs, shape, unI)
import           Generics.SOP.DMapUtilities (TypeListContains, TypeListTag (..),
                                             makeTypeListTagNP)

import           Data.Aeson                 (FromJSON (..), ToJSON (..), object,
                                             pairs, withObject, (.:), (.=))

tagToIndex::TypeListTag xs x -> Int
tagToIndex = go 0
  where
    go :: Int -> TypeListTag ys x -> Int
    go n TLHead           = n
    go n (TLTail tagTail) = go (n+1) tagTail

{-
indexToTag :: SListI xs => Int -> TypeListTag xs x
indexToTag n = go sList n
  where
    go :: SListI ys => SList ys -> Int -> TypeListTag ys x
    go SNil 0 = TLHead
    go SNil _ = error "impossible"
    go SCons 0 = error "impossible"
    go SCons n = TLTail $ go sList (n-1)

indexToTag :: forall xs x. (TypeListContains xs x ~ True) => Int -> TypeListTag xs x
indexToTag = go
  where
    go :: forall ys y. (TypeListContains ys y ~ True) => Int -> TypeListTag ys y
    go 0 = TLHead
    go n = TLTail $ go (n-1)

indexToTag :: forall xs x. SListI xs => Int -> TypeListTag xs x
indexToTag n = go sList n makeTypeListTagNP
  where
    go :: forall ys y. SListI ys => SList ys -> Int -> NP (TypeListTag ys) ys -> TypeListTag ys y
    go _ _ Nil = error "bad index (n > number of constructors?) in indexToTag."
    go SNil _ _ = error "bad index (n > number of constructors?) in indexToTag."
    go SCons 0 (t :* npTail) = t
    go SCons n (t :* npTail) = go sList (n-1) npTail

indexToTag :: ({- TypeListContains xs x ~ True,-} SListI xs) => [TypeListTag xs x]
indexToTag = go sList
  where
    safeIndex l n = if n >= 0 && n < length l then (Just $ l !! n) else Nothing
    go :: forall ys y. ({-TypeListContains ys y ~ True,-} SListI ys) => SList ys -> [TypeListTag ys y]
    go SNil = []
    go SCons = TLHead : (TLTail <$> go sList)
-}

data TagWrapper (xs :: [k]) where
  MkTagWrapper :: TypeListTag xs x -> TagWrapper xs

data TypeListLength (xs :: [k]) where
  Z :: Length '[]
  S :: Length xs -> Length (x ': xs)

indexToTag :: Int -> Length xs -> TagWrapper xs
indexToTag n = go sList n
  where
    go :: SList ys -> Int -> TagWrapper
    go SNil _  = error "End of type-list reached. Index larger than list?"
    go SCons 0 = MkTagWrapper TLHead
    go SCons n = TLTail $ go subShape (n-1)


indexToNS :: SListI xs => Int -> NS (TypeListTag xs) xs
indexToNS n = hapInjs makeTypeListTagNP !! n

-- since it's one value should we store as object or array?
instance ToJSON (TypeListTag xs x) where
  toJSON tag = object ["index" .= tagToIndex tag]
  toEncoding tag = pairs ("index" .= tagToIndex tag)

instance (TypeListContains xs x ~ True) => FromJSON (TypeListTag xs x) where
  parseJSON = withObject "TypeListTag" $ \v -> indexToTag <$> v .: "index"

