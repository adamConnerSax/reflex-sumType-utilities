{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
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
import           Generics.SOP               (All, All2, Code, Generic, I (..),
                                             K (..), NP (..), NS (..),
                                             Proxy (..), SList (..),
                                             SListI (..), Shape (..), from,
                                             hapInjs, hcmap, hcollapse, hmap,
                                             hsequence, shape, unI, unK, unSOP)
import           Generics.SOP.Constraint    (Compose)
import           Generics.SOP.DMapUtilities (TypeListContains, TypeListTag (..),
                                             dSumToNS, makeTypeListTagNP,
                                             nsToDSum)
import           Generics.SOP.NS            (collapse_NS, index_NS)
import qualified GHC.Generics               as GHCG

import           Data.Aeson                 (FromJSON (..), ToJSON (..), Value,
                                             encode, object, pairs, withObject,
                                             (.:), (.=))
import           Data.Functor.Identity      (Identity (Identity, runIdentity))
import qualified Data.HashMap.Lazy          as HM

import qualified Data.ByteString.Lazy       as LB

{-
instance All ToJSON xs => ToJSON (NS I xs) where
  toJSON ns =
    let toJSONC = Proxy :: Proxy ToJSON
        val = collapse_NS $ hcmap toJSONC (K . toJSON . unI) ns
    in  object ["tag" .= index_NS ns, "value" .= val]

instance All ToJSON xs => ToJSON (DS.DSum (TypeListTag xs) Identity) where
  toJSON = toJSON . hmap (I . runIdentity) . dSumToNS
-}


instance All (ToJSON `Compose` f) (xs :: [*]) => ToJSON (NS f xs) where
  toJSON ns =
    let toJSONC = Proxy :: Proxy (ToJSON `Compose` f)
        val = collapse_NS $ hcmap toJSONC (K . toJSON) ns
    in  object ["tag" .= index_NS ns, "value" .= val]

instance All (ToJSON `Compose` f) (xs :: [*]) => ToJSON (DS.DSum (TypeListTag xs) f) where
  toJSON = toJSON . dSumToNS


instance ToJSON a => ToJSON (I a) where
  toJSON = toJSON . unI

instance All ToJSON xs => ToJSON (NP I xs) where
  toJSON = let c = Proxy :: Proxy ToJSON in toJSON . hcollapse . hcmap c (K . toJSON . unI)

encodeGeneric :: (Generic a, All2 ToJSON (Code a), All (ToJSON `Compose` (NP I)) (Code a)) => a -> LB.ByteString
encodeGeneric = encode . unSOP . from

-- NB: This can fail if the index is >= length of the type-list, hence the Maybe return type
indexToNS :: All FromJSON xs => Int -> Value -> Maybe (NS (K Value) xs)
indexToNS n v = go sList n v
  where
    go :: SListI ys => SList ys -> Int -> Value -> Maybe (NS (K Value) ys)
    go SNil _ _ = Nothing -- "Bad index in indexToNS"
    go SCons 0 v = Just $ Z $ K v
    go SCons n v = go sList (n-1) v >>= Just . S

-- This could likely be written nore neatly with Either
instance All FromJSON xs => FromJSON (NS I xs) where
  parseJSON = withObject "NS I" $ \v -> do
    tag <- v .: "tag"
    case HM.lookup "value" v of
      Nothing -> fail "Failed to \"value\" when parsing (NS I xs)"
      Just unParsedValue ->
        let fromJSONC = Proxy :: Proxy FromJSON
        in case indexToNS tag unParsedValue of
          Just ns -> hsequence $ hcmap fromJSONC (parseJSON . unK) ns
          Nothing -> fail $ "index (" ++ show tag ++ ") indexToNS failed while parsing (NS I xs)."

instance All FromJSON xs => FromJSON (DS.DSum (TypeListTag xs) Identity) where
  parseJSON v = nsToDSum . hmap (Identity . unI) <$> parseJSON v


-- This makes a list of types into a type
-- empty list goes to ()
-- one-element list just goes to that type
-- n-element list (n > 1) goes to n-tuple
type family TypeListToType (xs :: [*]) :: * where
  TypeListToType '[] = ()
  TypeListToType (x ': '[]) = x
  TypeListToType (x ': ys) = (x, TypeListToType ys)

-- this makes a List of type-lists into a list of types using the above transformation
type family ListOfTypeListsToListOfTypes (xss :: [[*]]) :: [*] where
  ListOfTypeListsToListOfTypes '[] = '[]
  ListOfTypeListsToListOfTypes (xs ': yss) = TypeListToType xs ': ListOfTypeListsToListOfTypes yss

{-
npToTuple :: NP I xs -> TypeListToType xs
npToTuple Nil = ()
npToTuple (ix :* Nil) = unI ix
npToTuple (ix :* npTail) = (unI ix, npToTuple npTail)
-}
{-
toDSum :: Generic a => a -> DSum (TypeListTag (ListOfTypeListsToListOfTypes (Code a))) Identity
toDSum = from


-- an example

data Example = Ex1 A | Ex2 B | Ex3 Int Double deriving (GHCG.Generic)
instance Generic Example

data A = AC Int
data B = BC Char
-}

