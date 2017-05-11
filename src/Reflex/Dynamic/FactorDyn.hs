{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module Reflex.Dynamic.FactorDyn where

import Reflex (Reflex, MonadHold, Dynamic, Event, fmapMaybe, fforMaybe, never, switch, sample, current, updated, push, holdDyn, ffor, hold)
import Control.Monad.Fix (MonadFix)
import Data.Maybe (isJust)
import Data.Dependent.Sum (DSum ((:=>)))
import Data.GADT.Compare (GEq, (:~:) (Refl), geq)
import Data.Functor.Compose (Compose (Compose), getCompose)

import Generics.SOP.Distribute (distributeI_NP)
import Generics.SOP.DMapUtilities (dSumToNS, nsToDSum, nsOfnpUnCompose, FunctorWrapTypeListOfLists)
import Generics.SOP (Generic, Code, from, unSOP, SOP (SOP), to, hmap, hcmap, Proxy (Proxy), I (I), (:.:) (Comp), SListI)--(from, unSOP)


maybeDyn :: forall t a m. (Reflex t, MonadFix m, MonadHold t m) => Dynamic t (Maybe a) -> m (Dynamic t (Maybe (Dynamic t a)))
maybeDyn d = do
  ma <- sample $ current d
  let inner :: forall m'. (MonadFix m', MonadHold t m') => a -> m' (Dynamic t a)
      inner a = holdDyn a . fmapMaybe id =<< takeWhileE isJust (updated d)
  mInner0 :: Maybe (Dynamic t a) <- mapM inner ma
  rec result <- holdDyn mInner0 $ flip push (updated d) $ \new -> do
        old <- sample $ current d
        if isJust old == isJust new then return Nothing else Just <$> mapM inner new
  return result

takeWhileE :: forall t m a. (Reflex t, MonadFix m, MonadHold t m) => (a -> Bool) -> Event t a -> m (Event t a)
takeWhileE f e = do
  rec be <- hold e $ fforMaybe e' $ \a -> if f a
        then Just never
        else Nothing
      let e' = switch be
  return e'

--TODO: Is this a good name?  I'm not sure this really resembles factorization
factorDyn :: forall t m k (v :: [*] -> *). (Reflex t, MonadFix m, MonadHold t m, GEq k) => Dynamic t (DSum k v) -> m (Dynamic t (DSum k (Compose (Dynamic t) v)))
factorDyn d = do
  k0 :=> (v0 :: v a) <- sample $ current d
  let inner :: forall m' a. (MonadFix m', MonadHold t m') => k a -> v a -> m' (Dynamic t (v a))
      inner k v0 = holdDyn v0 . fmapMaybe id =<< takeWhileE isJust newVal
        where newVal = ffor (updated d) $ \(newK :=> newV) -> case newK `geq` k of
                Just Refl -> Just newV
                Nothing -> Nothing
  inner0 :: Dynamic t (v a) <- inner k0 v0
  rec result <- holdDyn (k0 :=> Compose inner0) $ flip push (updated d) $ \(newKey :=> newVal) -> do
        (oldKey :=> _) <- sample $ current d
        case newKey `geq` oldKey of
          Just Refl -> return Nothing
          Nothing -> do
            newInner <- inner newKey newVal
            return $ Just $ newKey :=> Compose newInner
  return result


factorDyn' :: forall t m a b. (Reflex t, MonadFix m, MonadHold t m, Generic a, Generic b, (Code b) ~ FunctorWrapTypeListOfLists (Dynamic t) (Code a))
  => Dynamic t a -> m (Dynamic t b)
factorDyn' da = do 
  let dSumDyn = nsToDSum . unSOP . from <$> da -- Dynamic t (DSum (TypeListTag (Code a)) (NP I xs))
  dSumDyn' <- factorDyn dSumDyn  -- Dynamic t (DSum (TypeListTag (Code a)) (NP I))
  let nsnpDyn = dSumToNS <$> dSumDyn' -- Dynamic t (NS (Compose (Dynamic t) (NP I)) (Code a))
      sListIC = Proxy :: Proxy SListI
      nsnpDyn' = hcmap sListIC (hmap (Comp. I) . distributeI_NP . getCompose) <$> nsnpDyn -- Dynamic t (NS (NP (I :.: Dynamic t)) (Code a))
      nsnpDyn'' = nsOfnpUnCompose <$> nsnpDyn' -- Dynamic t (NS (NP I) (Code b))
      result = to . SOP <$> nsnpDyn''
  return result


-- for example:
maybeDyn' :: (Reflex t, MonadHold t m, MonadFix m) => Dynamic t (Maybe a) -> m (Dynamic t (Maybe (Dynamic t a)))
maybeDyn' = factorDyn'

eitherDyn :: (Reflex t, MonadHold t m, MonadFix m) => Dynamic t (Either a b) -> m (Dynamic t (Either (Dynamic t a) (Dynamic t b)))
eitherDyn = factorDyn'
