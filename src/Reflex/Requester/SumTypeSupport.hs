{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-|
Module: Reflex.Requester.SumTypeSupport
Description: Aeson ToJSON and FromJSON instances for the TypeListTag type and DSum for use in websocket communication
-}
module Reflex.Requester.SumTypeSupport
  (
  ) where

import qualified Data.Dependent.Sum         as DS
import           Generics.SOP               (All, Code, Generic, I (..), K (..),
                                             NP (..), NS (..), Proxy (..),
                                             SList (..), SListI (..),
                                             Shape (..), hapInjs, hcmap, hmap,
                                             hsequence, shape, unI, unK)
import           Generics.SOP.DMapUtilities (TypeListContains, TypeListTag (..),
                                             dSumToNS, makeTypeListTagNP,
                                             nsToDSum)
import           Generics.SOP.NS            (collapse_NS, index_NS)

import           Data.Aeson                 (FromJSON (..), ToJSON (..), Value,
                                             object, pairs, withObject, (.:),
                                             (.=))
import           Data.Functor.Identity      (Identity (Identity, runIdentity))
import qualified Data.HashMap.Lazy          as HM


{-
tagToIndex::TypeListTag xs x -> Int
tagToIndex = go 0
  where
    go :: Int -> TypeListTag ys x -> Int
    go n TLHead           = n
    go n (TLTail tagTail) = go (n+1) tagTail

data TagWrapper (xs :: [*]) where
  MkTagWrapper :: TypeListTag xs x -> TagWrapper xs

indexToTag :: SListI xs => Int -> TagWrapper xs
indexToTag n = go sList n
  where
    go :: SList ys -> Int -> TagWrapper ys
    go SNil _  = error ""
    go SCons 0 = MkTagWrapper TLHead
    go SCons n = case go sList (n-1) of
      MkTagWrapper tail -> MkTagWrapper $ TLTail tail


-- since it's one value should we store as object or array?
instance ToJSON (TagWrapper xs) where
  toJSON (MkTagWrapper tag) = object ["index" .= tagToIndex tag]
  toEncoding (MkTagWrapper tag) = pairs ("index" .= tagToIndex tag)

instance SListI xs => FromJSON (TagWrapper xs) where
  parseJSON = withObject "TagWrapper" $ \v -> indexToTag <$> v .: "index"

class GToJSON t where
  gToJSON :: t a -> Value
--  gToEncoding :: t a -> Encoding

instance GToJSON (TypeListTag xs) where
  gToJSON tag = object ["index" .= tagToIndex tag]

class GToJSON tag => ToJSONTag tag f where
  toTaggedJSON :: tag a -> f a -> Value

-}

instance All ToJSON xs => ToJSON (NS I xs) where
  toJSON ns =
    let toJSONC = Proxy :: Proxy ToJSON
        val = collapse_NS $ hcmap toJSONC (K . toJSON . unI) ns
    in  object ["tag" .= index_NS ns, "value" .= val]

instance All ToJSON xs => ToJSON (DS.DSum (TypeListTag xs) Identity) where
  toJSON = toJSON . hmap (I . runIdentity) . dSumToNS


indexToNS :: All FromJSON xs => Int -> Value -> NS (K Value) xs
indexToNS n v = go sList n v
  where
    go :: SListI ys => SList ys -> Int -> Value -> NS (K Value) ys
    go SNil _ _ = error "Bad index in indexToNS"
    go SCons 0 v = Z $ K v
    go SCons n v = S $ go sList (n-1) v


instance All FromJSON xs => FromJSON (NS I xs) where
  parseJSON = withObject "NS I" $ \v -> do
    tag <- v .: "tag"
    let (Just unParsedValue) = HM.lookup "value" v
        fromJSONC = Proxy :: Proxy FromJSON
        ns = indexToNS tag unParsedValue
    hsequence $ hcmap fromJSONC (parseJSON . unK) ns

instance All FromJSON xs => FromJSON (DS.DSum (TypeListTag xs) Identity) where
  parseJSON v = nsToDSum . hmap (Identity . unI ) <$> parseJSON v

--instance ToJSONTag (TypeListTag xs) Identity where
--  toTaggedJSON tag ia = object [ "tag" .= gToJSON tag, "value" .= toJSON (runIdentity ia) ]


{-
instance All ToJSON xs => ToJSON (DS.DSum (TypeListTag xs) Identity) where
  toJSON (tag DS.:=> ia) = object ["tag" .= toJSON (MkTagWrapper tag)
                                  , "value" .= toJSON (runIdentity ia)
                                  ]
-}
