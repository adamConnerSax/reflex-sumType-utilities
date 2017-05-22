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
module Reflex.Requester.SumTypeSupport where

import qualified Data.Dependent.Sum         as DS
import           Generics.SOP               ((:.:) (Comp), All, All2, Code,
                                             Generic, I (..), K (..), NP (..),
                                             NS (..), Proxy (..), SList (..),
                                             SListI (..), SListI2, SOP (..),
                                             Shape (..), fn, from, hapInjs,
                                             hcliftA, hcmap, hcollapse, hcpure,
                                             hmap, hsequence, hsequence', shape,
                                             to, unI, unK, unSOP)
import           Generics.SOP.Constraint    (Compose)
import           Generics.SOP.Dict          (hdicts, unAll_POP, withDict)
import           Generics.SOP.DMapUtilities (TypeListContains, TypeListTag (..),
                                             dSumToNS, makeTypeListTagNP,
                                             nsToDSum)
import           Generics.SOP.NS            (ap_SOP, collapse_NS, index_NS,
                                             sequence'_NS)
import qualified GHC.Generics               as GHCG

import           Data.Aeson                 (FromJSON (..), ToJSON (..), Value,
                                             decode, encode, object, pairs,
                                             withObject, (.:), (.=))
import           Data.Aeson.Types           (Parser)
import           Data.Foldable              (foldl')
import           Data.Functor.Identity      (Identity (Identity, runIdentity))
import qualified Data.HashMap.Lazy          as HM

import qualified Data.ByteString.Lazy       as LB


instance All ToJSON xs => ToJSON (NP I xs) where
  toJSON = let c = Proxy :: Proxy ToJSON in toJSON . hcollapse . hcmap c (K . toJSON . unI)

instance All FromJSON xs => FromJSON (NP I xs) where
  parseJSON v = do
    valList <- parseJSONList v
    case buildNPKFromList valList of
      Just npVal -> hsequence $ hcmap (Proxy :: Proxy FromJSON) (parseJSON . unK) npVal
      Nothing -> fail "parsed list too short in FromJSON (NP I xs)"

buildNPKFromList :: SListI xs => [a] -> Maybe (NP (K a) xs)
buildNPKFromList as = go sList as
  where
    go :: SListI ys => SList ys -> [a] -> Maybe (NP (K a) ys)
    go SNil _  = Just Nil
    go SCons [] = Nothing
    go SCons (a : as') = case go sList as' of
      Just npTail -> Just $ K a :* npTail
      Nothing -> Nothing

instance (SListI2 xss, All2 ToJSON xss) => ToJSON (SOP I xss) where
  toJSON sop =
    let toJSONC = Proxy :: Proxy ToJSON
        val = hcollapse $ hcliftA toJSONC (K . toJSON . unI) sop
        index = index_NS $ unSOP sop
    in object ["tag" .= index, "value" .= val]

instance (SListI2 xss, All2 FromJSON xss) => FromJSON (SOP I xss) where
  parseJSON = withObject "SOP I xss" $ \v -> do
    index <- v .: "tag"
    listOfVals <- v .: "value"
    case indexToSOP index listOfVals of
      Nothing -> fail "Error in indexToSOP.  Could be bad index or val list too short."
      Just sopVal -> hsequence $ hcmap (Proxy :: Proxy FromJSON) (parseJSON . unK) sopVal


encodeGeneric :: (Generic a, All2 ToJSON (Code a)) => a -> LB.ByteString
encodeGeneric = encode . from

decodeGeneric :: (Generic a, All2 FromJSON (Code a)) => LB.ByteString -> Maybe a
decodeGeneric  = fmap to . decode

data Example = Ex1 A | Ex2 B | Ex3 Int Double deriving (GHCG.Generic, Show)
instance Generic Example

data A = AC Int deriving (GHCG.Generic, Show)
instance ToJSON A
instance FromJSON A

data B = BC Char deriving (GHCG.Generic, Show)
instance ToJSON B
instance FromJSON B

instance (All2 ToJSON xss, SListI2 xss) => ToJSON (DS.DSum (TypeListTag xss) (NP I)) where
  toJSON = toJSON . SOP . dSumToNS

instance (All2 FromJSON xss, SListI2 xss) => FromJSON (DS.DSum (TypeListTag xss) (NP I)) where
  parseJSON = fmap (nsToDSum . unSOP) . parseJSON

toDSum :: Generic a => a -> DS.DSum (TypeListTag (Code a)) (NP I)
toDSum = nsToDSum . unSOP . from

-- NB: This can fail if the index is >= length of the type-list, hence the Maybe return type
-- NB: This can fail if the list [a] is shorter then the type-list for the NP
indexToSOP :: SListI2 xss => Int -> [a] -> Maybe (SOP (K a) xss)
indexToSOP n xs = SOP <$> go sList n xs
  where
    go :: SListI2 yss => SList yss -> Int -> [a] -> Maybe (NS (NP (K a)) yss)
    go SNil _ _ = Nothing -- "Bad index in indexToNS"
    go SCons 0 xs = buildNPKFromList xs >>= Just . Z
    go SCons n xs = go sList (n-1) xs >>= Just . S



{-
instance All ToJSON xs => ToJSON (NS I xs) where
  toJSON ns =
    let toJSONC = Proxy :: Proxy ToJSON
        val = collapse_NS $ hcmap toJSONC (K . toJSON . unI) ns
    in  object ["tag" .= index_NS ns, "value" .= val]

instance All ToJSON xs => ToJSON (DS.DSum (TypeListTag xs) Identity) where
  toJSON = toJSON . hmap (I . runIdentity) . dSumToNS


instance All (ToJSON `Compose` f) (xs :: [*]) => ToJSON (NS f xs) where
  toJSON ns =
    let toJSONC = Proxy :: Proxy (ToJSON `Compose` f)
        val = collapse_NS $ hcmap toJSONC (K . toJSON) ns
    in  object ["tag" .= index_NS ns, "value" .= val]

instance All (ToJSON `Compose` f) (xs :: [*]) => ToJSON (DS.DSum (TypeListTag xs) f) where
  toJSON = toJSON . dSumToNS

instance ToJSON a => ToJSON (I a) where
  toJSON = toJSON . unI

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


-- NB: This can fail if the index is >= length of the type-list, hence the Maybe return type
indexToNS :: SListI xs => Int -> a -> Maybe (NS (K a) xs)
indexToNS n x = go sList n x
  where
    go :: SListI ys => SList ys -> Int -> a -> Maybe (NS (K a) ys)
    go SNil _ _ = Nothing -- "Bad index in indexToNS"
    go SCons 0 x = Just $ Z $ K x
    go SCons n x = go sList (n-1) x >>= Just . S



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
-}
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

-}

