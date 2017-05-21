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
